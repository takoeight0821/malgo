#!/usr/bin/env python3
"""Rewrite malgo record-literal/pattern field syntax from `{field = v}` to
`{.field -> v}` (malgo#434 -- unifies record syntax with codata's `.field`
projection sigil).

A single left-to-right scan tracks: string literals (`"..."`, with `\\`
escapes), line comments (`--`), block comments (`{- ... -}`, non-nesting --
matches the real lexer in lean/Malgo/Parser/Prim.lean), and a delimiter
stack ('{'/'('/'['). Entering a brace is classified as a record context
exactly the way the real parser does: an identifier immediately followed
by a lone `=` (not `==`, not `=>`) -- same signal `pRecordField`/
`isRecordStart` already key off. Once inside a record context, the same
check re-fires after each top-level `,` for the next field. Every matched
field's `ident = ` is rewritten to `.ident -> ` in place; nothing else in
the file is touched, so nested records, unrelated `def`/`let` bindings,
tuples (`{e1, e2}`, no `=`), type-level records (`{field: T}`, `:` not
`=`), and import selectors (`{..}`/`{foo, bar}`) are left alone.

Usage:
    python3 scripts/migrate-record-syntax.py [--check] PATH...

    --check   Report files that would change (exit 1 if any) without
              writing them. Otherwise rewrites files in place.

PATH may be a file or a directory (recursed for *.mlg).
"""

import sys
from pathlib import Path

IDENT_START = set("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_")
IDENT_CONT = IDENT_START | set("0123456789")
OP_CHARS = set("+-*/\\%=><:;|&!#.~")


def skip_ws_and_comments(s, i):
    """Advance i past whitespace, line comments, and block comments."""
    n = len(s)
    while i < n:
        c = s[i]
        if c in " \t\r\n":
            i += 1
        elif s.startswith("--", i):
            j = s.find("\n", i)
            i = n if j == -1 else j
        elif s.startswith("{-", i):
            j = s.find("-}", i + 2)
            if j == -1:
                return n  # unterminated block comment; let the compiler complain
            i = j + 2
        else:
            break
    return i


def read_ident(s, i):
    """If s[i:] starts with an identifier, return (ident, end_index); else None."""
    if i >= len(s) or s[i] not in IDENT_START:
        return None
    j = i + 1
    while j < len(s) and s[j] in IDENT_CONT:
        j += 1
    return s[i:j], j


def read_op_run(s, i):
    """Return (op_text, end_index) for the maximal run of operator chars at i."""
    j = i
    while j < len(s) and s[j] in OP_CHARS:
        j += 1
    return s[i:j], j


def check_record_field_start(s, i):
    """At position i (past '{' or a top-level ','), check for `ident '=' `
    (a lone `=`, not `==`/`=>`/etc). Returns (ident, ident_start, ident_end,
    eq_start, eq_end) or None."""
    j = skip_ws_and_comments(s, i)
    ident_hit = read_ident(s, j)
    if ident_hit is None:
        return None
    ident, ident_end = ident_hit
    k = skip_ws_and_comments(s, ident_end)
    op, op_end = read_op_run(s, k)
    if op != "=":
        return None
    return (ident, j, ident_end, k, op_end)


def migrate(text):
    """Return (new_text, changed) with every record field's `ident = `
    rewritten to `.ident -> `."""
    edits = []  # (start, end, replacement), start<end, non-overlapping, in order
    stack = []  # 'record' | 'other', one entry per currently-open '{'/'('/'['
    i = 0
    n = len(text)
    in_string = False

    while i < n:
        c = text[i]

        if in_string:
            if c == "\\" and i + 1 < n:
                i += 2
                continue
            if c == '"':
                in_string = False
            i += 1
            continue

        if c == '"':
            in_string = True
            i += 1
            continue

        if c == "'":
            # Char literal: 'x' or an escape like '\n'/'\''/'\\' -- always
            # exactly one logical character between quotes. Must be skipped
            # as an atomic unit: a literal like '{'/'['/'('/','/'"' would
            # otherwise be misread as a real delimiter and corrupt the
            # brace-depth stack for everything that follows in the file.
            j = i + 1
            if j < n and text[j] == "\\":
                j += 2
            else:
                j += 1
            if j < n and text[j] == "'":
                j += 1
            i = j
            continue

        if text.startswith("--", i):
            j = text.find("\n", i)
            i = n if j == -1 else j
            continue

        if text.startswith("{-", i):
            j = text.find("-}", i + 2)
            i = n if j == -1 else j + 2
            continue

        if c == "{":
            hit = check_record_field_start(text, i + 1)
            if hit is not None:
                ident, ident_start, ident_end, eq_start, eq_end = hit
                edits.append((ident_start, ident_start, "."))
                edits.append((eq_start, eq_end, "->"))
                stack.append("record")
                i = eq_end
                continue
            stack.append("other")
            i += 1
            continue

        if c in "([":
            stack.append("other")
            i += 1
            continue

        if c in "}])":
            if stack:
                stack.pop()
            i += 1
            continue

        if c == "," and stack and stack[-1] == "record":
            hit = check_record_field_start(text, i + 1)
            if hit is not None:
                ident, ident_start, ident_end, eq_start, eq_end = hit
                edits.append((ident_start, ident_start, "."))
                edits.append((eq_start, eq_end, "->"))
                i = eq_end
                continue
            i += 1
            continue

        i += 1

    if not edits:
        return text, False

    out = []
    pos = 0
    for start, end, replacement in edits:
        out.append(text[pos:start])
        out.append(replacement)
        pos = end
    out.append(text[pos:])
    return "".join(out), True


def iter_mlg_files(paths):
    for p in paths:
        path = Path(p)
        if path.is_dir():
            yield from sorted(path.rglob("*.mlg"))
        elif path.suffix == ".mlg":
            yield path
        else:
            yield path  # let it fail loudly if it's not actually text


def main(argv):
    check = "--check" in argv
    args = [a for a in argv if a != "--check"]
    if not args:
        print(__doc__, file=sys.stderr)
        return 2

    changed_files = []
    for path in iter_mlg_files(args):
        text = path.read_text()
        new_text, changed = migrate(text)
        if changed:
            changed_files.append(path)
            if not check:
                path.write_text(new_text)

    for path in changed_files:
        print(f"{'would change' if check else 'rewrote'}: {path}")
    print(f"{len(changed_files)} file(s) {'would change' if check else 'rewritten'}")

    return 1 if (check and changed_files) else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))

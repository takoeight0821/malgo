---
mode: "agent"
tools: ["codebase"]
---

#codebase

You are an experienced **Lean 4 compiler inspector**.  
Scan this repository (including sub-directories) and list **potential issues** you find.  
The project is a language processor (parser, type checker, IR transforms, code generators, etc.), so focus on problems typical of compiler development.

## Review Dimensions

1. **Compiler / Language-Processor Specific**
   - Ambiguous parser rules, left-recursion, precedence errors
   - Violations of AST invariants, inconsistencies between analysis passes
   - Infinite loops or worst-case exponential optimizations
2. **Lean-Specific Bugs**
   - `partial def` hiding a recursion that does not actually terminate
   - `panic!`/`!` indexing and other partial operations on untrusted input
   - `Option`/`Except` cases silently discarded (`getD`, `toOption`, a `catch`
     that swallows a real error)
   - Strictness: Lean's `match` is strict where the Haskell original relied on
     laziness to short-circuit
3. **Performance & Memory**
   - Unnecessary traversals or list construction in large IR passes
   - Quadratic re-scans in recursive traversals over nested IR nodes
   - `Std.TreeMap`/`HashMap` chosen where iteration order reaches output, or
     the reverse
4. **Effects & Errors**
   - `IO.Ref` state leaking across what should be isolated runs (uniq supply)
   - `EIO CompileError` errors converted to strings and losing their range
5. **Security**
   - Insufficient sandboxing when executing user-supplied code
   - Path-traversal in file I/O; command-injection risks
6. **Dependencies & Build Configuration**
   - `lakefile.toml` / `lean-toolchain` drift
   - `include_str` dependencies Lake does not track (see
     `lean/Malgo/Backend/Zig/Runtime.lean`)
7. **Test Coverage**
   - Missing `#guard` assertions on pure functions
   - Lack of golden tests for printers / formatters

## Output Format

For each **category**, use a heading `### <Category>` and list issues with:

1. **Title** (short)
2. **File / Line** (if known)
3. **Description** (≤ 4 lines)
4. **Recommended Fix** (bullets allowed)

## Constraints

- **Analyze only files whose names end with `.lean`.**
- Skip generated or external dirs: `lean/.lake`, `.git`, `vendor`, `result`, etc.
- Mark uncertain findings with phrases like “might be”.
- Cap the list at **100 items**, ordered by severity (Critical → Low).
- Finish with a `## Summary` section that re-lists the **top 5 issues to address first**.

Start with a concise overall summary, then provide the detailed list.

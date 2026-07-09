#!/usr/bin/env python3
"""Correlates a Malgo Zig-backend MALGO_RC_TRACE=1 JSON-lines log with the
compile-time symbolic names the trace already carries, to answer "who still
holds a reference to this object right now" without manually grepping raw
pointer addresses.

Usage:
    MALGO_RC_TRACE=1 ./some_compiled_binary 2>trace.jsonl
    python3 scripts/rctrace.py trace.jsonl                    # first dropReuse_miss
    python3 scripts/rctrace.py trace.jsonl --target 0x104d604c0
    python3 scripts/rctrace.py trace.jsonl --target 0x104d604c0 --at-line 812

The trace format (emitted by runtime/zig/runtime.zig's "Named RC tracing"
section) is one JSON object per line:
  {"ev":"dup"|"drop"|"dropReuse_attempt"|"dropReuse_hit"|"dropReuse_miss",
   "ptr":"0x..","rc":N,"name":"..","func":".."}
  {"ev":"mkStruct"|"mkClosure"|"mkStructReuse","ptr":"0x..","func":"..",
   "slots":[{"i":0,"name":"..","child":"0x.."}, ...]}
  {"ev":"decChild","container":"0x..","slot":0,"child":"0x..","rc_before":N}
  {"ev":"trace_overflow"}  -- a name/func string didn't fit runtime.zig's
  fixed trace-line buffer; this marker takes the dropped event's place so a
  missing event is visible instead of silent (see emitTraced there).
"""

import argparse
import json
import sys

DIRECT_EVENTS = {"dup", "drop", "dropReuse_attempt", "dropReuse_hit", "dropReuse_miss"}
CONSTRUCT_EVENTS = {"mkStruct", "mkClosure", "mkStructReuse"}


def load_events(path):
    handle = sys.stdin if path == "-" else open(path, "r", encoding="utf-8")
    try:
        for lineno, raw in enumerate(handle, 1):
            raw = raw.strip()
            if not raw:
                continue
            try:
                ev = json.loads(raw)
            except json.JSONDecodeError:
                continue
            ev["_line"] = lineno
            yield ev
    finally:
        if handle is not sys.stdin:
            handle.close()


def find_default_target(events):
    """Picks the first dropReuse_miss event as the thing worth explaining."""
    for ev in events:
        if ev.get("ev") == "dropReuse_miss":
            return ev
    return None


def find_generation(events, target, at_line):
    """The Value heap reuses freed addresses for unrelated later allocations,
    so `target` is only a stable identity within one "generation": the span
    from its most recent construction event at or before `at_line` up to
    (but not including) the next one, if any. Returns (start_line, end_line)
    -- end_line is None if this is the last generation seen for `target`.
    Every other lookup below must be scoped to this range, or a `dropReuse`
    on one generation could be misattributed to an event from a completely
    different, already-dead object that happened to reuse the same address.
    """
    start = None
    end = None
    for ev in events:
        if ev.get("ev") in CONSTRUCT_EVENTS and ev.get("ptr") == target:
            if ev["_line"] <= at_line:
                start = ev["_line"]
                end = None
            elif start is not None and end is None:
                end = ev["_line"]
    return start, end


def live_referrers_at(events, target, start_line, at_line):
    """Replays construction/decChild events in [start_line, at_line] and
    returns the set of (container, slot) entries that still hold a
    reference to `target` -- i.e. were captured by a construction event and
    never released by a matching decChild."""
    live = {}
    for ev in events:
        if ev["_line"] < start_line:
            continue
        if ev["_line"] > at_line:
            break
        kind = ev.get("ev")
        if kind in CONSTRUCT_EVENTS:
            container = ev.get("ptr")
            func = ev.get("func")
            for slot in ev.get("slots", []):
                key = (container, slot.get("i"))
                if slot.get("child") == target:
                    live[key] = {
                        "name": slot.get("name"),
                        "func": func,
                        "container": container,
                        "slot": slot.get("i"),
                        "line": ev["_line"],
                    }
                elif key in live:
                    # This slot was overwritten by a different child (e.g. a
                    # mkStructReuse recycling the same container), so
                    # whatever it used to hold is no longer referenced here.
                    del live[key]
        elif kind == "decChild":
            key = (ev.get("container"), ev.get("slot"))
            if key in live and ev.get("child") == target:
                del live[key]
    return list(live.values())


def print_timeline(events, target, start_line, end_line):
    print(f"--- direct RC events on {target}, this generation only (line {start_line}..{end_line or 'end'}) ---")
    found_any = False
    for ev in events:
        if ev["_line"] < start_line:
            continue
        if end_line is not None and ev["_line"] >= end_line:
            break
        if ev.get("ev") in DIRECT_EVENTS and ev.get("ptr") == target:
            found_any = True
            print(
                f"  line {ev['_line']:>6}  {ev['ev']:<20} rc={ev.get('rc', '?'):<4} "
                f"name={ev.get('name')!r:<40} func={ev.get('func')!r}"
            )
    if not found_any:
        print("  (none)")


def print_birth(events, target, start_line):
    for ev in events:
        if ev["_line"] == start_line and ev.get("ev") in CONSTRUCT_EVENTS and ev.get("ptr") == target:
            print(f"--- {target} (this generation) was constructed at line {start_line} ({ev['ev']}, func={ev.get('func')!r}) ---")
            return
    print(f"--- {target}'s construction event was not found in this trace (allocated before tracing was relevant, or via mk{{Int32,String,...}}) ---")


def count_overflows(events, start_line, end_line):
    """How many trace_overflow markers (see runtime.zig's emitTraced) fall in
    [start_line, end_line) -- a construction event lost to one could have
    been a referrer of `target` that every lookup below now can't see."""
    total = 0
    for ev in events:
        if ev["_line"] < start_line:
            continue
        if end_line is not None and ev["_line"] >= end_line:
            break
        if ev.get("ev") == "trace_overflow":
            total += 1
    return total


def print_live_referrers(events, target, start_line, at_line):
    referrers = live_referrers_at(events, target, start_line, at_line)
    overflows = count_overflows(events, start_line, at_line + 1)
    print(f"--- live referrers of {target} at/before line {at_line} (this generation) ---")
    if overflows:
        print(f"  WARNING: {overflows} trace_overflow marker(s) in this range -- a lost")
        print("  construction event could have been a referrer this trace can no")
        print("  longer identify; the result below may be incomplete, not definitive.")
    if not referrers:
        if overflows:
            print("  (none found in the events this trace could decode -- see the")
            print("   trace_overflow warning above before concluding it is unreferenced)")
        else:
            print("  (none found -- either truly unreferenced, or held only as a")
            print("   direct local variable never captured into a struct/closure;")
            print("   see the direct RC event timeline above for that case)")
        return
    for r in referrers:
        print(
            f"  container={r['container']} slot={r['slot']} name={r['name']!r} "
            f"captured-by-func={r['func']!r} (captured at line {r['line']})"
        )


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("trace", nargs="?", default="-", help="path to a MALGO_RC_TRACE=1 JSONL log, or - for stdin")
    parser.add_argument("--target", help="hex pointer (e.g. 0x104d604c0) to investigate; defaults to the first dropReuse_miss")
    parser.add_argument("--at-line", type=int, help="line number to snapshot live referrers at; defaults to the target's own dropReuse_miss/last event")
    args = parser.parse_args()

    events = list(load_events(args.trace))
    if not events:
        print("no events read from trace", file=sys.stderr)
        return 1

    total_overflows = sum(1 for ev in events if ev.get("ev") == "trace_overflow")
    if total_overflows:
        print(
            f"warning: {total_overflows} trace_overflow marker(s) in this trace -- some "
            "events (whose name/func string overflowed runtime.zig's fixed trace buffer) "
            "were replaced by this marker at the source, so this trace may be incomplete",
            file=sys.stderr,
        )

    target = args.target
    at_line = args.at_line
    if target is None:
        miss = find_default_target(events)
        if miss is None:
            print("no --target given and no dropReuse_miss event found in this trace", file=sys.stderr)
            return 1
        target = miss["ptr"]
        at_line = at_line or miss["_line"]
        print(f"(no --target given; using first dropReuse_miss: {target} at line {miss['_line']}, name={miss.get('name')!r}, func={miss.get('func')!r})")

    if at_line is None:
        matching = [ev["_line"] for ev in events if ev.get("ptr") == target]
        at_line = matching[-1] if matching else events[-1]["_line"]

    start_line, end_line = find_generation(events, target, at_line)
    if start_line is None:
        print(f"warning: no construction event for {target} at or before line {at_line} -- treating the whole trace as one generation", file=sys.stderr)
        start_line = 1

    print_birth(events, target, start_line)
    print()
    print_timeline(events, target, start_line, end_line)
    print()
    print_live_referrers(events, target, start_line, at_line)
    return 0


if __name__ == "__main__":
    sys.exit(main())

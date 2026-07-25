/-! Port of `src/Malgo/Backend/Zig/Runtime.hs`: embeds the checked-in Zig
runtime (`runtime/zig/runtime.zig`) into the compiler at build time, so the
emitted program is a single, hermetic `.zig` file (no relative-path
`@import` needed at build time).

Haskell uses Template Haskell's `embedStringFile`; Lean uses the builtin
`include_str` term, resolved relative to this file's own directory
(`lean/Malgo/Backend/Zig/`), four levels up to the repo root, then into
`runtime/zig/runtime.zig`.

**Lake does not track `include_str` as a build dependency** (verified: editing
`runtime.zig` and rebuilding leaves this module's `.olean` untouched, so the
compiler keeps emitting the *previous* runtime text). Haskell has no such
problem — `embedStringFile` calls `addDependentFile`. After changing
`runtime.zig`, force this module to rebuild:

```
rm -f lean/.lake/build/lib/lean/Malgo/Backend/Zig/Runtime.olean \
      lean/.lake/build/lib/lean/Malgo/Backend/Zig/Runtime.trace \
      lean/.lake/build/ir/Malgo/Backend/Zig/Runtime.c
```

`touch`ing this file is *not* enough. A change that touches `runtime.zig` and
nothing else is the dangerous case: a stale build produces a green
`lean-zig-golden` run that tested the old runtime. -/

namespace Malgo.Backend.Zig

def zigRuntime : String := include_str "../../../../runtime/zig/runtime.zig"

end Malgo.Backend.Zig

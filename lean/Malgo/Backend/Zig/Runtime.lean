/-! Port of `src/Malgo/Backend/Zig/Runtime.hs`: embeds the checked-in Zig
runtime (`runtime/zig/runtime.zig`) into the compiler at build time, so the
emitted program is a single, hermetic `.zig` file (no relative-path
`@import` needed at build time).

Haskell uses Template Haskell's `embedStringFile`; Lean uses the builtin
`include_str` term, resolved relative to this file's own directory
(`lean/Malgo/Backend/Zig/`), four levels up to the repo root, then into
`runtime/zig/runtime.zig`.

**Lake's tracking of `include_str` is not reliable.** Measured on the same
edit, twice: changing `runtime.zig` rebuilt this module, but *reverting* that
change did not — `lake build` reported success while the produced binary went
on embedding the previous runtime text. Haskell has no such problem;
`embedStringFile` calls `addDependentFile`, and the same revert rebuilt and
cleared correctly there. After changing `runtime.zig`, force this module to
rebuild rather than trusting the build to notice:

```
rm -f lean/.lake/build/lib/lean/Malgo/Backend/Zig/Runtime.olean \
      lean/.lake/build/lib/lean/Malgo/Backend/Zig/Runtime.trace \
      lean/.lake/build/ir/Malgo/Backend/Zig/Runtime.c
```

`touch`ing this file is *not* enough. A change that touches `runtime.zig` and
nothing else is the dangerous case: a stale build produces a green
`lean-zig-golden` run that tested the old runtime. CI does the removal
unconditionally before its sweep (see `.github/workflows/lean.yml`), and
`mise run lean-bust-runtime` does the same locally. -/

namespace Malgo.Backend.Zig

def zigRuntime : String := include_str "../../../../runtime/zig/runtime.zig"

end Malgo.Backend.Zig

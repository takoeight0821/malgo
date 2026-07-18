/-! Port of `src/Malgo/Backend/Zig/Runtime.hs`: embeds the checked-in Zig
runtime (`runtime/zig/runtime.zig`) into the compiler at build time, so the
emitted program is a single, hermetic `.zig` file (no relative-path
`@import` needed at build time).

Haskell uses Template Haskell's `embedStringFile`; Lean uses the builtin
`include_str` term, resolved relative to this file's own directory
(`lean/Malgo/Backend/Zig/`), four levels up to the repo root, then into
`runtime/zig/runtime.zig`. -/

namespace Malgo.Backend.Zig

def zigRuntime : String := include_str "../../../../runtime/zig/runtime.zig"

end Malgo.Backend.Zig

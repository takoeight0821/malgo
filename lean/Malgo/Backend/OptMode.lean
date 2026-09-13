/-! The `--opt` level, shared by every backend `malgo compile` can target.

The three names are the user-facing vocabulary; what each one means is the
toolchain's business (`Backend/Zig/Toolchain.lean`, `Backend/Go/Toolchain.lean`),
and the two disagree — Zig has real optimization levels, Go has one code
generator and a debug switch. -/

namespace Malgo.Backend

inductive OptMode where
  | debug
  | releaseSafe
  | releaseFast
  deriving BEq, Repr

def parseOptMode : String → Except String OptMode
  | "debug" => .ok .debug
  | "release-safe" => .ok .releaseSafe
  | "release-fast" => .ok .releaseFast
  | m => .error s!"Unknown --opt mode: {m}"

end Malgo.Backend

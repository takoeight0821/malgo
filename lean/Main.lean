import Malgo

def main (_args : List String) : IO UInt32 := do
  IO.eprintln "malgo (Lean port): the CLI lands with milestone M1; run `lake test` for the M0 gate."
  return 1

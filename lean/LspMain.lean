import Malgo.LSP

/-! Port of `malgo-lsp/app/Main.hs`: the `malgo-lsp` executable's entry
point. Takes no CLI arguments/flags. -/

def main : IO UInt32 := Malgo.LSP.runLSP

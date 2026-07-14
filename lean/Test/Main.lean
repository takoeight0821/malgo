import Malgo
import Test.Golden

/-! M0 test driver. The gate: hand-built `SExpr` values must render
byte-identically to fragments of the committed hspec goldens, proving the
s-cargot printer port before any compiler pass exists. -/

namespace Malgo.Test

open Malgo

private def sym (s : String) : SExpr := .atom (.symbol s)
private def str (s : String) : SExpr := .atom (.str s)
private def int (n : Int) : SExpr := .atom (.int n none)

/-- Hand transcription of `test/testcases/malgo/Primitive.mlg`'s parse
golden (`.golden/Malgo.Parser/Primitive/golden`, 442 bytes). -/
private def primitiveParseSExpr : SExpr :=
  .list [sym "module",
    str "test/testcases/malgo/Primitive.mlg",
    .list [
      .list [sym "import", str "runtime/malgo/Builtin.mlg", sym "all"],
      .list [sym "def", sym "main",
        .list [sym "fn",
          .list [
            .list [sym "clause",
              .list [sym "_"],
              .list [sym "seq",
                .list [sym "do",
                  .list [sym "apply", sym "printString#",
                    .list [sym "apply", sym "toStringInt64#",
                      .list [sym "apply",
                        .list [sym "apply", sym "addInt64#",
                          .list [sym "int64", int 40]],
                        .list [sym "int64", int 2]]]]]]]]]]]]

def cases : List GoldenCase := [
  { group := "Malgo.Parser", name := "Primitive", run := pure (sShow primitiveParseSExpr) }
]

end Malgo.Test

def main (args : List String) : IO UInt32 := do
  match Malgo.Test.parseArgs args with
  | .error e =>
    IO.eprintln e
    return 2
  | .ok cfg =>
    Malgo.Test.runSuite cfg Malgo.Test.cases

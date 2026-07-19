import Malgo.Monad

/-! Port of `src/Malgo/Pass.hs`. The Haskell `Pass` class exists only to
wrap each pass's error type into `CompileError`; the Lean port keeps the
convention (one entry point per pass module) plus this helper. -/

namespace Malgo

/-- Run a pass body that throws its own error type, wrapping failures
into the uniform `CompileError`. -/
def wrapError (passName : String) (render : ε → String) (rangeOf : ε → Option Range)
    (act : ExceptT ε MalgoM α) : MalgoM α := do
  match ← act.run with
  | .ok a => return a
  | .error e => throw { passName, message := render e, range? := rangeOf e }

end Malgo

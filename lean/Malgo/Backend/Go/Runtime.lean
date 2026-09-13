/-! The Go runtime, embedded as text.

**After editing `runtime/go/runtime.go`, run `mise run bust-runtime` before
rebuilding.** Lake does not reliably track `include_str`: `lake build` can
report success while the compiled binary keeps emitting the previous runtime
text, so a golden sweep passes against a runtime that is no longer on disk.
`touch` is not enough; the `.olean`, `.trace` and `.c` artifacts for this
module have to go. CI does it unconditionally in the Go golden job. -/

namespace Malgo.Backend.Go

/-- Contents of `runtime/go/runtime.go`, a package-clause-free fragment.
`Emit` supplies the `package main` line and the import block. -/
def goRuntime : String := include_str "../../../../runtime/go/runtime.go"

end Malgo.Backend.Go

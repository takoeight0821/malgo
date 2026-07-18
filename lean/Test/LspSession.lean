import Malgo.LSP.Json

/-! A scripted stdio session against the real `malgo-lsp` binary:
initialize → didOpen a broken file → assert a `publishDiagnostics`
notification with a real diagnostic → shutdown → exit.

Haskell's `malgo-lsp` has no such test (confirmed: no `LSPSpec.hs`, and
`build-lsp.yml`'s CI job only checks that the executable compiles) — the
project's own design doc lists an integration test like this as a
未実装 ("not yet implemented") future item. There is nothing to port
byte-for-byte; this is authored fresh against the wire-format details
established while porting `Malgo.LSP.{Json,Protocol,Server.JsonRpc}`. -/

namespace Malgo.Test.LspSession

open Malgo.LSP.Json

private def lspBinPath : System.FilePath := System.FilePath.mk "lean/.lake/build/bin/malgo-lsp"

/-- Wall-clock budget for the whole scripted session. Bounds a hang in
`malgo-lsp` (a future regression in `checkFile`/`fetchRenamedModule`, or a
build that never notices `exit`) to a fast, diagnosable failure instead of
silently blocking `lake test` until CI's blanket 30-minute job timeout. -/
private def sessionTimeoutMs : UInt32 := 20000

private def frame (v : JValue) : ByteArray :=
  let body := (encodeJson v).toUTF8
  (s!"Content-Length: {body.size}\r\n\r\n").toUTF8 ++ body

/-- Read the `Content-Length` header, skipping other header lines until the
blank separator. Returns `none` on EOF *or* on a malformed (non-numeric)
`Content-Length` value — the latter is logged first, since collapsing both
into the same `none` would silently make a protocol violation
indistinguishable from a clean disconnect at the `readMessage` call site
(mirroring the same fix applied to `Malgo.LSP.Server.JsonRpc`). -/
private partial def readContentLength (h : IO.FS.Stream) : IO (Option Nat) := do
  let rec go : IO (Option Nat) := do
    let line ← h.getLine
    if line.isEmpty then return none
    let stripped := line.trimAsciiEnd.toString
    if stripped.isEmpty then go
    else if stripped.startsWith "Content-Length: " then
      let rest := (stripped.drop "Content-Length: ".length).trimAscii.toString
      skipBlank
      match rest.toNat? with
      | some n => return some n
      | none =>
        IO.eprintln s!"Malgo.Test.LspSession: malformed Content-Length value: {rest}"
        return none
    else go
  go
where
  skipBlank : IO Unit := do
    let line ← h.getLine
    if line.isEmpty then return ()
    if line.trimAsciiEnd.toString.isEmpty then return () else skipBlank

private partial def readExactly (h : IO.FS.Stream) (n : Nat) : IO ByteArray := do
  let rec loop (acc : ByteArray) (remaining : Nat) : IO ByteArray := do
    if remaining == 0 then return acc
    let chunk ← h.read (USize.ofNat remaining)
    if chunk.size == 0 then return acc
    loop (acc ++ chunk) (remaining - chunk.size)
  loop ByteArray.empty n

private def readMessage (h : IO.FS.Stream) : IO (Option JValue) := do
  match ← readContentLength h with
  | none => return none
  | some len =>
    let body ← readExactly h len
    match String.fromUTF8? body with
    | none => return none
    | some text => return decodeJson text

/-- The actual scripted protocol exchange, factored out of `run` so process
cleanup can be unconditional: `return` inside a `do` block exits the
*enclosing function*, so the many early `.error` returns below (one per
assertion) — if they lived directly inside `run` — would each skip any
cleanup code placed after them. As a separate top-level `def`, they only
return from `sessionSteps`; `run` captures whatever value comes back and
runs cleanup regardless of which branch produced it. -/
private def sessionSteps (stdin stdout : IO.FS.Stream) (content uri : String) :
    IO (Except String Unit) := do
  -- initialize
  stdin.write (frame (jObject [("jsonrpc", .string "2.0"), ("id", .number 1),
    ("method", .string "initialize"), ("params", .object [])]))
  stdin.flush
  let some initResp ← readMessage stdout | return .error "no response to initialize"
  let some hoverProvider := jLookup "result" initResp >>= jLookup "capabilities" >>= jLookup "hoverProvider" >>= jBool
    | return .error s!"initialize response missing capabilities.hoverProvider: {repr initResp}"
  unless hoverProvider do return .error "hoverProvider capability is not true"

  -- initialized
  stdin.write (frame (jObject [("jsonrpc", .string "2.0"), ("method", .string "initialized"),
    ("params", .object [])]))
  stdin.flush

  -- didOpen a broken file
  stdin.write (frame (jObject [("jsonrpc", .string "2.0"), ("method", .string "textDocument/didOpen"),
    ("params", jObject [("textDocument", jObject
      [("uri", .string uri), ("languageId", .string "malgo"), ("version", .number 1),
       ("text", .string content)])])]))
  stdin.flush

  let some diagMsg ← readMessage stdout | return .error "no publishDiagnostics notification"
  let some method := jLookup "method" diagMsg >>= jText
    | return .error s!"malformed notification: {repr diagMsg}"
  unless method == "textDocument/publishDiagnostics" do
    return .error s!"expected textDocument/publishDiagnostics, got {method}"
  let some diags := jLookup "params" diagMsg >>= jLookup "diagnostics" >>= jArray
    | return .error s!"publishDiagnostics missing diagnostics array: {repr diagMsg}"
  if diags.isEmpty then return .error "expected at least one diagnostic for a broken file"
  for d in diags do
    let some severity := jLookup "severity" d >>= jInt | return .error s!"diagnostic missing severity: {repr d}"
    unless severity == 1 do return .error s!"expected severity 1, got {severity}"
    let some src := jLookup "source" d >>= jText | return .error s!"diagnostic missing source: {repr d}"
    unless src == "malgo" do return .error s!"expected source \"malgo\", got {src}"
    let some msg := jLookup "message" d >>= jText | return .error s!"diagnostic missing message: {repr d}"
    if msg.isEmpty then return .error "diagnostic message is empty"

  -- shutdown
  stdin.write (frame (jObject [("jsonrpc", .string "2.0"), ("id", .number 2),
    ("method", .string "shutdown"), ("params", .object [])]))
  stdin.flush
  let some shutdownResp ← readMessage stdout | return .error "no response to shutdown"
  let some shutdownId := jLookup "id" shutdownResp >>= jInt
    | return .error s!"shutdown response missing id: {repr shutdownResp}"
  unless shutdownId == 2 do return .error s!"expected shutdown response id 2, got {shutdownId}"

  -- exit (process lifecycle — waiting for the exit code and reaping the
  -- child — is `run`'s responsibility, so the whole thing gets cleaned up
  -- uniformly regardless of whether this session succeeded or failed)
  stdin.write (frame (jObject [("jsonrpc", .string "2.0"), ("method", .string "exit"),
    ("params", .object [])]))
  stdin.flush
  return .ok ()

/-- Run the scripted session; returns `.ok ()` on success or `.error msg`
describing the first assertion that failed.

The child's stderr is inherited (not piped-and-never-drained — a classic
subprocess deadlock risk if the server ever logs enough to fill an unread
OS pipe buffer, and its output is more useful visible directly in the test
run than silently discarded anyway). A watchdog task kills the child if the
whole exchange overruns `sessionTimeoutMs`. Cleanup (`kill` + `wait`, to
reap the process and recover its exit code) always runs, whether
`sessionSteps` returned `.ok`, `.error`, or raised an `IO.Error`. -/
def run : IO (Except String Unit) := do
  let brokenFixture := System.FilePath.mk "test/testcases/malgo/error/ErrorInvalidIdent.mlg"
  unless (← brokenFixture.pathExists) do
    return .error s!"fixture not found: {brokenFixture}"
  let content ← IO.FS.readFile brokenFixture
  let uri := s!"file://{← IO.FS.realPath brokenFixture}"

  let child ← IO.Process.spawn
    { cmd := lspBinPath.toString,
      stdin := .piped, stdout := .piped, stderr := .inherit }
  let stdin := IO.FS.Stream.ofHandle child.stdin
  let stdout := IO.FS.Stream.ofHandle child.stdout

  let _watchdog ← IO.asTask do
    IO.sleep sessionTimeoutMs
    child.kill

  let result ←
    try sessionSteps stdin stdout content uri
    catch e => pure (.error (toString e))

  -- Reap the child regardless of outcome. `kill` on an already-exited
  -- process is harmless (any error from it is discarded); `wait` on an
  -- already-reaped child just returns its cached exit code.
  (try child.kill catch _ => pure ())
  let exitCode ← child.wait

  match result with
  | .error msg => return .error msg
  | .ok () =>
    unless exitCode == 0 do return .error s!"expected exit code 0, got {exitCode}"
    return .ok ()

end Malgo.Test.LspSession

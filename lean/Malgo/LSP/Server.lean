import Std.Data.TreeMap
import Malgo.LSP.Json
import Malgo.LSP.Protocol
import Malgo.LSP.Server.JsonRpc

/-! Port of `malgo-lsp/src/Malgo/LSP/Server.hs`: a lightweight LSP server
loop over stdin/stdout. -/

namespace Malgo.LSP.Server

open Malgo.LSP
open Malgo.LSP.Json
open Malgo.LSP.Server.JsonRpc

/-- The server environment, accessible to all handlers. -/
structure ServerEnv where
  envStdout : IO.FS.Stream
  envVfs : IO.Ref (Std.TreeMap NormalizedUri String)
  envLogger : String → IO Unit

/-- Respond to the `initialize` request with server capabilities: full-sync
`textDocumentSync` (open/close, no incremental `willSave`, no save-with-text)
and `hoverProvider: true`. Nothing else is advertised — no
`definitionProvider`, `completionProvider`, etc. (matching the Haskell
server's actual, minimal surface — see `Handlers.lean`'s module doc). -/
def handleInitialize (env : ServerEnv) (msg : JsonRpcMessage) : IO Unit := do
  let capabilities : JValue :=
    jObject
      [ ("capabilities",
          jObject
            [ ("textDocumentSync",
                jObject
                  [ ("openClose", .bool true),
                    ("change", .number 1),
                    ("willSave", .bool false),
                    ("willSaveWaitUntil", .bool false),
                    ("save", jObject [("includeText", .bool false)]) ]),
              ("hoverProvider", .bool true) ]) ]
  match msg.id with
  | some reqId => sendResponse env.envStdout reqId capabilities
  | none => pure ()

/-- Main message dispatch loop. Terminates on `exit` or when `readMessage`
returns `none` (EOF/connection closed). After `shutdown`, every message
other than `exit` is silently dropped (matching Haskell — the LSP spec says
to reject with an error, but this implementation just ignores). -/
partial def serverLoop (env : ServerEnv) (dispatch : ServerEnv → JsonRpcMessage → IO Unit) :
    IO Unit := go false
where
  go (shutdown : Bool) : IO Unit := do
    match ← readMessage (← IO.getStdin) with
    | none => pure ()
    | some msg =>
      match msg.method with
      | "initialize" => handleInitialize env msg; go shutdown
      | "initialized" => go shutdown
      | "shutdown" =>
        match msg.id with
        | some reqId => sendResponse env.envStdout reqId .null
        | none => pure ()
        go true
      | "exit" => pure ()
      | _ =>
        if shutdown then pure ()
        else do dispatch env msg; go shutdown

/-- Run the LSP server. Returns an exit code (always 0 — matching Haskell,
which has no failure path out of `serverLoop`). -/
def runServer (dispatch : ServerEnv → JsonRpcMessage → IO Unit) : IO UInt32 := do
  let vfs ← IO.mkRef ({} : Std.TreeMap NormalizedUri String)
  let stdout ← IO.getStdout
  let env : ServerEnv :=
    { envStdout := stdout, envVfs := vfs,
      envLogger := fun msg => IO.eprintln msg }
  serverLoop env dispatch
  return 0

/-- Send `textDocument/publishDiagnostics`. -/
def publishDiagnostics (env : ServerEnv) (nuri : NormalizedUri) (diags : List Diagnostic) : IO Unit :=
  sendNotification env.envStdout "textDocument/publishDiagnostics" <|
    jObject
      [ ("uri", .string (fromNormalizedUri nuri).unUri),
        ("diagnostics", .array (diags.map encodeDiagnostic)) ]

def getFileContent (env : ServerEnv) (nuri : NormalizedUri) : IO (Option String) := do
  return (← env.envVfs.get).get? nuri

def updateFileContent (env : ServerEnv) (nuri : NormalizedUri) (content : String) : IO Unit :=
  env.envVfs.modify (·.insert nuri content)

def removeFileContent (env : ServerEnv) (nuri : NormalizedUri) : IO Unit :=
  env.envVfs.modify (·.erase nuri)

/-- Log a message to stderr (never sent to the client as `window/logMessage`
— matching Haskell). -/
def logMessage (env : ServerEnv) (msg : String) : IO Unit := env.envLogger msg

end Malgo.LSP.Server

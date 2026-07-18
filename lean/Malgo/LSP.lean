import Malgo.LSP.Handlers

/-! Port of `malgo-lsp/src/Malgo/LSP.hs`: the LSP server entry point and
JSON-RPC dispatch table. -/

namespace Malgo.LSP

open Malgo
open Malgo.LSP.Json
open Malgo.LSP.Server
open Malgo.LSP.Server.JsonRpc

/-- Parse `didOpen` params: extract URI and text content. -/
def parseDidOpen (msg : JsonRpcMessage) : Option (Uri × String) := do
  let params ← msg.params
  let td ← jLookup "textDocument" params
  let uriText ← jLookup "uri" td >>= jText
  let text ← jLookup "text" td >>= jText
  pure (Uri.mk uriText, text)

/-- Parse `didChange` params: extract URI and optional full text (only
`contentChanges[0].text` is ever read — full-document sync). -/
def parseDidChange (msg : JsonRpcMessage) : Option (Uri × Option String) := do
  let params ← msg.params
  let td ← jLookup "textDocument" params
  let uriText ← jLookup "uri" td >>= jText
  let mText : Option String := do
    let changes ← jLookup "contentChanges" params
    let arr ← jArray changes
    match arr with
    | first :: _ => jLookup "text" first >>= jText
    | [] => none
  pure (Uri.mk uriText, mText)

/-- Parse `didClose` params: extract URI. -/
def parseDidCloseUri (msg : JsonRpcMessage) : Option Uri := do
  let params ← msg.params
  let td ← jLookup "textDocument" params
  let uriText ← jLookup "uri" td >>= jText
  pure (Uri.mk uriText)

/-- Parse `hover` params: extract URI and position. -/
def parseHover (msg : JsonRpcMessage) : Option (Uri × LspPosition) := do
  let params ← msg.params
  let td ← jLookup "textDocument" params
  let uriText ← jLookup "uri" td >>= jText
  let posVal ← jLookup "position" params
  let line ← jLookup "line" posVal >>= jInt
  let char ← jLookup "character" posVal >>= jInt
  pure (Uri.mk uriText, { line, character := char })

/-- Dispatch an incoming JSON-RPC message to the appropriate handler.
Anything not one of these 4 methods is logged and ignored (no error
response) — matching Haskell's minimal surface. -/
def dispatch (state : LspState) (env : ServerEnv) (msg : JsonRpcMessage) : IO Unit := do
  match msg.method with
  | "textDocument/didOpen" =>
    match parseDidOpen msg with
    | none => logMessage env "Failed to parse didOpen params"
    | some (uri, content) => handleDidOpen env state uri content
  | "textDocument/didChange" =>
    match parseDidChange msg with
    | none => logMessage env "Failed to parse didChange params"
    | some (uri, mText) => handleDidChange env state uri mText
  | "textDocument/didClose" =>
    match parseDidCloseUri msg with
    | none => logMessage env "Failed to parse didClose params"
    | some uri => handleDidClose env state uri
  | "textDocument/hover" =>
    match parseHover msg with
    | none =>
      match msg.id with
      | some reqId => sendResponse env.envStdout reqId .null
      | none => pure ()
    | some (uri, pos) => do
      let nuri := toNormalizedUri uri
      let result ← handleHover state nuri pos
      match msg.id with
      | some reqId => sendResponse env.envStdout reqId result
      | none => pure ()
  | other => logMessage env s!"Unknown method: {other}"

/-- Run the Malgo LSP server over stdin/stdout. -/
def runLSP : IO UInt32 := do
  let db ← Malgo.Query.newQueryDB
  let renamedCache ← IO.mkRef ({} : Std.TreeMap NormalizedUri (ModuleName × Syntax.Module .rename))
  let state : LspState := { db, renamedCache }
  runServer (dispatch state)

end Malgo.LSP

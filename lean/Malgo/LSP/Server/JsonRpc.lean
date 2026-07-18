import Malgo.LSP.Json

/-! Minimal JSON-RPC 2.0 message framing over stdio.

Port of `Malgo.LSP.Server.JsonRpc` (`malgo-lsp/src/Malgo/LSP/Server/JsonRpc.hs`).
Transport only — no protocol semantics. `Content-Length` counts UTF-8 *bytes*,
not codepoints, so bodies are measured/read as `ByteArray`. -/

namespace Malgo.LSP.Server.JsonRpc

open Malgo.LSP.Json

/-- An incoming JSON-RPC message (request or notification). `id` is `none` for
    notifications. -/
structure JsonRpcMessage where
  id : Option JValue
  method : String
  params : Option JValue
  deriving Repr

/-- Parse a `JsonRpcMessage` from a `JValue`. -/
def parseMessage : JValue → Option JsonRpcMessage
  | .object kvs => do
    let m ← match List.lookup "method" kvs with
      | some (.string t) => some t
      | _ => none
    some { id := List.lookup "id" kvs, method := m, params := List.lookup "params" kvs }
  | _ => none

/-- Read exactly `n` bytes from a handle, looping over short reads. Returns
    fewer bytes only on EOF. -/
private partial def readExactly (h : IO.FS.Stream) (n : Nat) : IO ByteArray := do
  let rec loop (acc : ByteArray) (remaining : Nat) : IO ByteArray := do
    if remaining == 0 then return acc
    let chunk ← h.read (USize.ofNat remaining)
    if chunk.size == 0 then return acc
    loop (acc ++ chunk) (remaining - chunk.size)
  loop ByteArray.empty n

/-- Read the `Content-Length` header, skipping other header lines until the
    blank separator. Returns `none` on EOF. -/
private partial def readContentLength (h : IO.FS.Stream) : IO (Option Nat) := do
  let line ← h.getLine
  if line.isEmpty then return none
  let stripped := line.trimAsciiEnd.toString
  if stripped.isEmpty then
    readContentLength h
  else if stripped.startsWith "Content-Length: " then
    let rest := (stripped.drop "Content-Length: ".length).trimAscii.toString
    skipUntilBlank h
    return rest.toNat?
  else
    readContentLength h
where
  skipUntilBlank (h : IO.FS.Stream) : IO Unit := do
    let line ← h.getLine
    if line.isEmpty then return ()
    if line.trimAsciiEnd.toString.isEmpty then return () else skipUntilBlank h

/-- Read one `Content-Length`-framed JSON-RPC message from a handle. Returns
    `none` on EOF (how "connection closed" propagates to the caller's loop). -/
def readMessage (h : IO.FS.Stream) : IO (Option JsonRpcMessage) := do
  match ← readContentLength h with
  | none => return none
  | some len =>
    let body ← readExactly h len
    if body.size == 0 then return none
    match String.fromUTF8? body with
    | none => return none
    | some text => return decodeJson text >>= parseMessage

/-- Encode a JSON value with `Content-Length` framing and write to a handle.
    The length is the body's UTF-8 *byte* count, and the handle is flushed
    after every message. -/
def sendMessage (h : IO.FS.Stream) (val : JValue) : IO Unit := do
  let bodyBytes := (encodeJson val).toUTF8
  h.putStr s!"Content-Length: {bodyBytes.size}\r\n\r\n"
  h.write bodyBytes
  h.flush

/-- Send a JSON-RPC response (for a request with an id). -/
def sendResponse (h : IO.FS.Stream) (reqId result : JValue) : IO Unit :=
  sendMessage h <| jObject
    [ "jsonrpc" .= .string "2.0",
      "id" .= reqId,
      "result" .= result ]

/-- Send a JSON-RPC notification (server → client, no id). -/
def sendNotification (h : IO.FS.Stream) (method : String) (params : JValue) : IO Unit :=
  sendMessage h <| jObject
    [ "jsonrpc" .= .string "2.0",
      "method" .= .string method,
      "params" .= params ]

end Malgo.LSP.Server.JsonRpc

#r "packages/Suave/lib/net40/Suave.dll"
open System
open System.IO
open Suave
open Suave.Web
open Suave.Types
open Suave.Http
open Suave.Http.Successful
open Suave.Http.RequestErrors
open Suave.Http.Applicatives

// #2
// TODO: What is the ChatMessage that chat agent handles?
// TODO: Implement chat agent to store the room state
// (Format messages as "<li><strong>%s</strong>: %s</li>")

type ChatMessage =
  | Post of string * string
  | Get of AsyncReplyChannel<string>

let startChat() =
  MailboxProcessor.Start(fun inbox ->
    let rec loop msgs = async {
      let! msg = inbox.Receive()
      match msg with
      | Post(n, t) ->
          let h = sprintf "<li><strong>%s</strong>: %s</li>" n t
          return! loop (h::msgs)
      | Get(repl) ->
          repl.Reply(String.concat "\n" msgs)
          return! loop msgs }
    loop [] )

// ------------------------------------------------------------------

type AgentDict = Map<string, MailboxProcessor<ChatMessage>>

type RouterMessage =
  | Send of string * ChatMessage
  | List of AsyncReplyChannel<string>

let (|MapFind|_|) map key = Map.tryFind key map

let router =
  MailboxProcessor.Start(fun inbox ->
    let rec loop (agents:AgentDict) = async {
      let! msg = inbox.Receive()
      match msg with
      | List(repl) ->
          [ for KeyValue(k, _) in agents ->
              sprintf "<li><a href='/%s/'>%s</a></li>" k k ]
          |> String.concat "" |> repl.Reply
          return! loop agents
      | Send(MapFind agents agent, msg) ->
          agent.Post(msg)
          return! loop agents
      | Send(room, msg) ->
          let agent = startChat()
          agent.Post(msg)
          return! loop (Map.add room agent agents) }
    loop Map.empty )

// #3
// DEMO: Add support for multiple chat rooms
// DEMO: Add routing for multiple rooms

// #1
// DEMO: Add handlers for REST API
// TODO: Handle /chat with GET & no chache using getMessage
// TODO: Handle /post with POST & no cache using postMessage
// TODO: Otherwise, report NOT_FOUND
let getMessages room ctx = async {
  let! body = router.PostAndAsyncReply(fun r -> Send(room, Get(r)))
  let html = "<ul>" + body + "</ul>"
  return! OK html ctx }

let postMessage room ctx = async {
  let name = ctx.request.url.Query.Substring(1)
  use sr = new StreamReader(new MemoryStream(ctx.request.rawForm))
  let text = sr.ReadToEnd()
  router.Post(Send(room, Post(name, text)))
  return! ACCEPTED "OK" ctx }

let getRooms ctx = async {
  let! html = router.PostAndAsyncReply(List)
  return! OK html ctx }

// ------------------------------------------------------------------

let noCache =
  Writers.setHeader "Cache-Control" "no-cache, no-store, must-revalidate"
  >>= Writers.setHeader "Pragma" "no-cache"
  >>= Writers.setHeader "Expires" "0"

let index = File.ReadAllText(__SOURCE_DIRECTORY__ + "/web/chat.html")
let app =
  choose
    [ path "/" >>= Writers.setMimeType "text/html" >>= OK index
      path "/chat" >>= GET >>= noCache >>= getMessages "Home"
      path "/post" >>= POST >>= noCache >>= postMessage "Home"
      path "/rooms" >>= GET >>= noCache >>= getRooms

      pathScan "/%s/" (fun _ -> Writers.setMimeType "text/html" >>= OK index)
      pathScan "/%s/chat" (fun room -> GET >>= noCache >>= getMessages (room.Trim('/')))
      pathScan "/%s/post" (fun room -> POST >>= noCache >>= postMessage (room.Trim('/')))

      ]








//

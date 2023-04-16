import std/asynchttpserver
import std/asyncdispatch
import json, uri, strutils, os

import messagedb, types
import ".."/[pge, variabledata, matrix]

import nimja

type ServerData = ref object
  db: MessageDB
  addresses: seq[(string, uint16)]

proc handleComet(req: Request, s: ServerData) {.async.} =
  var stream = ""
  var index = 0
  for key, value in req.url.query.decodeQuery:
    case key:
    of "stream": stream = value
    of "index": index = parseInt(value)
  if stream == "":
    await req.respond(Http404, "Invalid stream")
  var messagesFuture = s.db.getSince(stream, index)
  let headers = {"Content-type": "application/json; charset=utf-8"}

  if not await messagesFuture.withTimeout(30 * 1000):
    await req.respond(Http200, """{"messages": []}""", headers.newHttpHeaders())
  else:
    let (messages, index) = messagesFuture.read()
    let data = %* {
        "messages": messages,
        "index": index
      }
    await req.respond(Http200, $data, headers.newHttpHeaders())

proc handleSendData(req: Request, s: ServerData) {.async.} =
  let stream = generateMessageStreamID()
  let dataobj = req.body.parseJson().to(DataObj)
  let data = Data(
    trainX: dataobj.trainX.toVariableData(),
    approxX: dataobj.approxX.toVariableData(),
    trainY: dataobj.trainY.vector(),
    approxY: dataobj.approxY.vector(),
    errorBound: dataobj.errorBound
  )

  let headers = {"Content-type": "text/plain; charset=utf-8"}
  await req.respond(Http200, stream, headers.newHttpHeaders())

  let f = calculatePge(data, s.addresses, proc(str: string) =
    echo ">    ", str
    s.db.add(stream, str)
    )
  f.callback = proc () =
    if f.failed:
      if f.error of SufferingFromSuccessException:
        discard
      else:
        raise f.error

  

const TemplateDir = currentSourcePath() / ".." / "templates"
const StaticDir = currentSourcePath() / ".." / ".." / ".." / "bin" / "web" / "static"

proc renderMain(): string =
  compileTemplateFile(TemplateDir / "main.html")

proc handleMain(req: Request) {.async.} =
  let headers = {"Content-type": "text/html; charset=utf-8"}
  await req.respond(Http200, renderMain(), headers.newHttpHeaders())

proc renderProgress(streamId: string): string =
  compileTemplateFile(TemplateDir / "progress.html")

proc handleProgress(req: Request) {.async.} =
  var stream = ""
  for key, value in req.url.query.decodeQuery:
    case key:
    of "stream": stream = value
  
  let headers = {"Content-type": "text/html; charset=utf-8"}
  await req.respond(Http200, renderProgress(stream), headers.newHttpHeaders())

proc handleStatic(req: Request, filename: string) {.async.} =
  let (_, _, ext) = filename.splitFile()
  let mime = case ext:
    of ".js": "text/javascript"
    of ".css": "text/css"
    else: "text/plain"
  let headers = {"Content-type": mime & "; charset=utf-8"}
  try:
    echo StaticDir / filename
    let f = readFile(StaticDir / filename)
    await req.respond(Http200, f, headers.newHttpHeaders())
  except IOError:
    await req.respond(Http404, "File not found", headers.newHttpHeaders())

proc handle(req: Request, s: ServerData) {.async, gcsafe.} =
  {.gcsafe.}: # DO NOT USE WITH MULTITHREADED CODE, THIS IS NOT ACTUALLY SAFE
    try: # TODO: add static files
      if req.url.path == "/comet" and req.reqMethod == HttpGet:
        await handleComet(req, s)
      elif req.url.path == "/senddata" and req.reqMethod == HttpPost:
        await handleSendData(req, s)
      elif req.url.path == "/" and req.reqMethod == HttpGet:
        await handleMain(req)
      elif req.url.path == "/progress" and req.reqMethod == HttpGet:
        await handleProgress(req)
      elif req.url.path.startsWith("/static/") and req.reqMethod == HttpGet:
        let filename = req.url.path["/static/".len..^1]
        await handleStatic(req, filename)
      else:
        await req.respond(Http404, "Page not found")
    except:
      await req.respond(Http500, "Internal server error")

proc serverRun*(addresses: seq[(string, uint16)]) {.async.} =
  var server = newAsyncHttpServer()
  var serverData = ServerData()
  serverData.db = initMessageDB()
  serverData.addresses = addresses

  proc cb(r: Request) {.async.} =
    await handle(r, serverData)

  server.listen(Port(5000))
  while true:
    if server.shouldAcceptRequest():
      await server.acceptRequest(cb)
    else:
      await sleepAsync(500)

when isMainModule:
  when RemoteEvaluation:
    let params = commandLineParams()
    if params.len == 0:
      echo "Usage: webserver ADDRESS:PORT ..."
    var addresses = newSeq[(string, uint16)]()
    for p in params:
      let s = p.split(':', 2)
      addresses.add (s[0], s[1].parseInt.uint16)
  else:
    let addresses = newSeq[(string, uint16)]()
  waitFor serverRun(addresses)
import message
import std/[asyncnet, asyncdispatch, asyncstreams, options]
import ".."/[variabledata, matrix, varpro, formula]
from ".."/jit/jit import AllowSimd, simdWidth
import ".."/jit/simddata
from os import commandLineParams
from strutils import parseInt

var queue {.threadvar.}: FutureStream[Message]
var client {.threadvar.}: AsyncSocket

proc processQueue() {.async.} =
  when AllowSimd:
    var x, xApprox: SimdData
  else:
    var x, xApprox: VariableData
  var y, yApprox: Vector
  var howMany, howManyApprox: int
  while true:
    let (b, m) = await queue.read()
    if not b: break
    if m.isTree:
      let (tree, id, isApprox) = m.decodeTree()
      # debugEcho id, ": ", queue.len
      let data = if isApprox:
          tree.fitParams(x, y, howMany)
        else:
          tree.fitParams(xApprox, yApprox, howManyApprox)
      let r = encodeResponse(id, data)
      asyncCheck client.send(r)
    else:
      let (xNew, yNew, howManyNew, isApprox) = m.decodeProblemSet()
      if isApprox:
        when AllowSimd:
          (x, y, howMany) = (xNew.toSimd(simdWidth), yNew, howManyNew)
        else:
          (x, y, howMany) = (xNew, yNew, howManyNew)
        debugEcho "got approximate data"
      else:
        when AllowSimd:
          (xApprox, yApprox, howManyApprox) = (xNew.toSimd(simdWidth), yNew, howManyNew)
        else:
          (xApprox, yApprox, howManyApprox) = (xNew, yNew, howManyNew)
        debugEcho "got exact data"

proc serve(port: uint16) {.async.} =
  asyncCheck processQueue()
  var server = newAsyncSocket()
  server.setSockOpt(OptReuseAddr, true)
  server.bindAddr(Port(port))
  server.listen()

  echo "Server started!"
  while true:
    client = await server.accept()
    echo "Client accepted!"
    
    while true:
      let m = await client.recvMessage()
      if m.isNone: break
      asyncCheck queue.write(m.get)

when isMainModule:
  let params = commandLineParams()
  if params.len != 1:
    echo "Usage: evalServer PORT"
  queue = newFutureStream[Message]("evalServer")
  asyncCheck serve(parseInt(params[0]).uint16)
  runForever()
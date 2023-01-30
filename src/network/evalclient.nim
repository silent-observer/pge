import asyncnet, asyncdispatch, tables, options
import message
from ../varpro import VarProResult
import ".."/[variabledata, matrix, formula]

var waitingQueue: Table[TreeId, Future[VarProResult]]
var servers: seq[AsyncSocket]
var waitSizes: seq[int]

const WaitingThreshold = 20
var waitingFuture: Future[void] = nil

proc readLoop(i: int) {.async.} =
  while true:
    let r = await servers[i].recvResponse()
    if r.isNone: break
    let (t, id) = r.get.decodeResponse()
    waitingQueue[id].complete(t)
    waitingQueue.del id
    dec waitSizes[i]
    if not waitingFuture.isNil and waitingQueue.len < WaitingThreshold:
      waitingFuture.complete()
      waitingFuture = nil

proc waitUntilProcessed(): Future[void] =
  if waitingQueue.len < WaitingThreshold:
    result = newFuture[void]("waitUntilProcessed")
    result.complete()
  else:
    if waitingFuture.isNil:
      waitingFuture = newFuture[void]("waitUntilProcessed")
    result = waitingFuture

proc initClient*(addresses: seq[(string, uint16)]) =
  waitingQueue = initTable[TreeId, Future[VarProResult]]()
  for i, (address, port) in addresses.pairs:
    servers.add (waitFor asyncnet.dial(address, Port(port)))
    waitSizes.add 0
    asyncCheck readLoop(i)

proc setRemoteData*(isApprox: bool, x: VariableData, y: Vector, howMany=30) =
  #result = newFuture[Void]("setRemoteData")
  let m = encodeProblemSet(isApprox, x, y, howMany)
  # echo "Sending data!"
  for server in servers:
    waitFor server.send(m)
  # echo "Sent data!"

proc sendTree(f: LinearFormula, isApprox: bool): Future[VarProResult] =
  result = newFuture[VarProResult]("fitParamsRemote")
  var id = TreeId(1)
  while id in waitingQueue:
    inc id
  waitingQueue[id] = result
  let m = encodeTree(f, id, isApprox)

  var minIndex = 0
  for i in 1..<waitSizes.len:
    if waitSizes[i] < waitSizes[minIndex]:
      minIndex = i
    
  inc waitSizes[minIndex]
  waitFor servers[minIndex].send(m)

proc fitParamsRemote*(f: LinearFormula, isApprox: bool):
    Future[VarProResult] {.async.} =
  waitFor waitUntilProcessed()
  result = await f.sendTree(isApprox)
# This is just an example to get you started. A typical binary package
# uses this file as the main entry point of the application.

#import nimprof
import expressions, formula, treegenerator, trie, ppq, varpro, matrix, variabledata
import simplifier
import random, math, times, std/monotimes
import strformat
import streams
import tables, sequtils, algorithm
import network/evalclient
import asyncdispatch

from os import commandLineParams
from strutils import parseInt, split

const VarCount = 4

type Data = object
  trainX, approxX: VariableData
  trainY, approxY: Vector

const PeekCoefficient = 0.1
const EchoIntermediate = false

proc generateData(): Data =
  const N = 100
  const PeekN = int(PeekCoefficient * N.float)
  result.trainX = initVariableData(VarCount)
  result.approxX = initVariableData(VarCount)
  result.trainY = vector(N)
  result.approxY = vector(PeekN)
  for i in 0..<N:
    let x = rand(1.0..4.0).Number
    let y = rand(1.0..4.0).Number
    let z = rand(1.0..4.0).Number
    let w = rand(1.0..4.0).Number
    # let err = gauss(sigma=0.001).Number
    #let f = x.pow(2) + y.pow(2) - 2 * x * y + 0.5 * x - 1.5 * y + 3
    #let f = exp(x) - 0.5 * exp(-0.5 * x)
    # let f = -x + y/(x.pow(2) + y.pow(2) + 0.1) + 1.0
    #let f = exp(0.3 * x + 0.2 * y) + exp(0.3 * y) - 0.5
    let f = x*y*y + exp(x)/((1+w))
    result.trainX.add @[x, y, z, w]
    result.trainY[i] = f
    if result.approxX.rows < PeekN:
      result.approxY[result.approxX.rows] = f
      result.approxX.add @[x, y, z, w]

var exprSet = newTrie[TotalKinds + VarCount]()
var mainQueue, approxQueue: ParetoPriorityQueue
var front: ParetoFront
var startTime: Monotime
var functionsFit = 0

# var treeState = initTable[int, string]()


const RemoteEvaluation = false

var dataArray: array[bool, (VariableData, Vector, int)]

proc approxTree(f: LinearFormula) {.async.} =
  let s = f.serialized()

  if s in exprSet:
    when EchoIntermediate:
      echo "X ", f
    # treeState[f.id] = "#f1faee"
    return
  when EchoIntermediate:
    echo "A ", f
  # treeState[f.id] = "#3a86ff"

  when RemoteEvaluation:
    let t = await f.fitParamsRemote(isApprox=true)
  else:
    let t = f.fitParams(dataArray[true][0], dataArray[true][1], dataArray[true][2])
  let comp = s.complexity()
  approxQueue.add f, t.error, comp

# let file = openFileStream("graph4.csv", fmWrite)
# let fileMeta = openFileStream("graph4meta.csv", fmWrite)

proc handleTree(f: LinearFormula) {.async.} =
  let s = f.serialized()

  if s in exprSet:
    when EchoIntermediate:
      echo "X ", f
    # treeState[f.id] = "#f1faee"
    return

  when EchoIntermediate:
    echo "> ", f
  # treeState[f.id] = "#4f772d"

  #let f = e.linearize()
  exprSet.add s
  when RemoteEvaluation:
    let t = await f.fitParamsRemote(isApprox=false)
  else:
    let t = f.fitParams(dataArray[false][0], dataArray[false][1], dataArray[false][2])

  # echo t.linearParams
  # echo t.nonlinearParams
  let text =
    if t.error > 1e10: $f
    else: f.toString(t.linearParams, t.nonlinearParams)
  let comp = s.complexity()
  inc functionsFit

  if t.error < 1e-6:
    echo ""
    echo ""
    echo "!!! Found solution (very small error) !!!"
    echo text
    echo fmt"Error: {t.error:.3e}"
    echo fmt"Log error: {log10(t.error):.2f}"
    echo fmt"Complexity: {comp}"
    echo fmt"Functions evaluated: {functionsFit}"
    echo fmt"Total time: {getMonoTime() - startTime}"
    echo fmt"Average fills: {totalFills.float / totalTries.float}"
    echo fmt"Average steps: {totalSteps.float / totalTries.float}"
    echo fmt"Total tries: {totalTries}"
    # file.close()

    # fileMeta.writeLine "id;node_color"
    # for t in treeState.pairs.toSeq().sorted():
    #   fileMeta.writeLine t[0], ";", t[1]
    # fileMeta.close()
    quit 0
  front.add text, t.error, comp
  mainQueue.add f, t.error, comp
  #echo " -> ", log10(t.error)

proc checkSuddenDrop() =
  let data = front.front

  var maxSlope = 0.0
  var maxI = 0
  echo ""
  echo "Slopes:"
  for i in 1..<data.len:
    let
      prevLogError = log10(data[i-1].error)
      thisLogError = log10(data[i].error)
      slope = (thisLogError - prevLogError) / 
        Number(data[i-1].complexity - data[i].complexity)
    echo fmt"{i}{'\t'}{slope:.2f}{'\t'}{data[i].text}"
    if slope > maxSlope:
      maxSlope = slope
      maxI = i
  
  if maxSlope > 0.85:
    let d = data[maxI]
    echo ""
    echo ""
    echo "!!! Found solution (high slope) !!!"
    echo d.text
    echo fmt"Error: {d.error:.3e}"
    echo fmt"Log error: {log10(d.error):.2f}"
    echo fmt"Complexity: {d.complexity}"
    echo fmt"Functions evaluated: {functionsFit}"
    echo fmt"Total time: {getMonoTime() - startTime}"
    quit 0
  else:
    let d = data[maxI]
    echo ""
    echo "Best:"
    echo fmt"Error: {d.error:.3e}"
    echo fmt"Log error: {log10(d.error):.2f}"
    echo fmt"Complexity: {d.complexity}"
    echo fmt"Functions evaluated: {functionsFit}"
    echo ""

proc main(addresses: seq[(string, uint16)]) {.async.} =
  echo "Start!"
  initClient(addresses)
  echo "Sucessfully connected!"

  let data = generateData()

  startTime = getMonoTime()
  const basis = {
    BasisFunction.Exp,
    BasisFunction.Inverse,
    BasisFunction.IntPower
  }
  
  dataArray[false] = (data.trainX, data.trainY, 30)
  dataArray[true] = (data.approxX, data.approxY, 10)
  setRemoteData(false, dataArray[false][0], dataArray[false][1], dataArray[false][2])
  setRemoteData(true, dataArray[true][0], dataArray[true][1], dataArray[true][2])

  let emptyFormula = initLinearFormula()
  await emptyFormula.handleTree()
  
  var approxFutures = newSeq[Future[void]]()
  var exactFutures = newSeq[Future[void]]()
  # file.writeLine "source;target"
  block main:
    #file.writeLine("Hello world")
    #file.close()

    for i in 0..<1000:
      let n = mainQueue.pop()
      # treeState[n.tree.id] = "#ffc300"
      echo i, ": ", n.tree
      echo fmt"Error: {n.error:.3e}"
      echo fmt"Log error: {log10(n.error):.2f}"
      echo fmt"Complexity: {n.complexity}"

      for tree in n.tree.generateFormulas(basis, VarCount):
        # tree.simplify().handleTree(data)
        approxFutures.add tree.simplify().approxTree()
        # file.writeLine(n.tree.id, ";", tree.id)
      # if i < 5:
      await all(approxFutures)

      const ApproxQueueCoefficient = 0.4
      let totalQueueSize = mainQueue.len + approxQueue.len
      let requiredMainQueue = ceil(totalQueueSize.float * ApproxQueueCoefficient).int
      let toTake = requiredMainQueue - mainQueue.len
      for _ in 0..<toTake:
        let node = approxQueue.pop()
        exactFutures.add node.tree.handleTree()
      # if mainQueue.len == 0:
      await all(exactFutures)
      # else:
        # asyncCheck all(exactFutures)

      echo ""
      
      if i mod 25 == 24:
        checkSuddenDrop()

when isMainModule:
  let params = commandLineParams()
  if params.len == 0:
    echo "Usage: pge ADDRESS:PORT ..."
  var addresses = newSeq[(string, uint16)]()
  for p in params:
    let s = p.split(':', 2)
    addresses.add (s[0], s[1].parseInt.uint16)
  waitFor main(addresses)
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
import options

from os import commandLineParams
from strutils import parseInt, split, parseFloat

const VarCount = 1

type Data* = object
  trainX*, approxX*: VariableData
  trainY*, approxY*: Vector
  errorBound*: float

type PgeResult* = object
  text*: string
  error*, logError*: float
  complexity*: int
  functionsFit*: int
  totalTime*: Duration

type SufferingFromSuccessException* = object of CatchableError
  pgeResult*: PgeResult

const PeekCoefficient = 0.1
const EchoIntermediate = true
const RemoteEvaluation* = false

proc generateData(): Data =
  const N = 100
  const PeekN = int(PeekCoefficient * N.float)
  result.trainX = initVariableData(VarCount)
  result.approxX = initVariableData(VarCount)
  result.trainY = vector(N)
  result.approxY = vector(PeekN)
  # result.errorBound = 0.1
  for i in 0..<N:
    let x = rand(0.0..4.0).Number
    # let y = exp(rand(-2.0..2.0).Number)
    # let z = rand(1.0..4.0).Number
    # let w = rand(1.0..4.0).Number
    let err = gauss(sigma=0.1).Number
    # let f = x.pow(2) + y.pow(2) - 2 * x * y + 0.5 * x - 1.5 * y + 3
    #let f = exp(x) - 0.5 * exp(-0.5 * x)
    # let f = -x + y/(x.pow(2) + y.pow(2) + 0.1) + 1.0 + err
    # let f = x/y + err
    # let f = ln(x)
    # let f = 1/(1/x + y/z)
    # let f = exp(-x*x/(2*y))/sqrt(2*PI*y)
    let f = exp(-(x - 2)*(x - 2) / 0.25)
    #let f = exp(0.3 * x + 0.2 * y) + exp(0.3 * y) - 0.5
    # let f = x*y*y + exp(x)/((1+w))
    #let f = exp(-0.2*x)*sin(5*x) + x*y
    result.trainX.add @[x]
    result.trainY[i] = f
    if result.approxX.rows < PeekN:
      result.approxY[result.approxX.rows] = f
      result.approxX.add @[x]

var exprSet = newTrie[TotalKinds + VarCount]()
var mainQueue, approxQueue: ParetoPriorityQueue
var front: ParetoFront
var startTime: Monotime
var functionsFit = 0

# var treeState = initTable[int, string]()

proc readData(filename: string): Data =
  var f = open(filename)
  defer: f.close()

  let N = f.readLine().parseInt()
  let PeekN = min(10, int(PeekCoefficient * N.float))

  result.trainX = initVariableData(VarCount)
  result.approxX = initVariableData(VarCount)
  result.trainY = vector(N)
  result.approxY = vector(PeekN)
  for i in 0..<N:
    let l = f.readLine().split('\t')
    result.trainX.add @[l[0].parseFloat()]
    result.trainY[i] = l[1].parseFloat()
  var indexList = toSeq(0..<N)
  indexList.shuffle()
  for i in 0..<PeekN:
    let j = indexList[i]
    result.approxX.add @[result.trainX[j][0]]
    result.approxY[i] = result.trainY[j]


var dataArray: array[bool, (VariableData, Vector, int)]
var expectedError = 1e-7

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
  # treeState[f.id] = "#4f772d"

  #let f = e.linearize()
  exprSet.add s
  when RemoteEvaluation:
    let t = await f.fitParamsRemote(isApprox=false)
  else:
    let t = f.fitParams(dataArray[false][0], dataArray[false][1], dataArray[false][2])
    # await sleepAsync(1000)

  # echo t.linearParams
  # echo t.nonlinearParams
  let text =
    if t.error > 1e9: $f
    else: f.toString(t.linearParams, t.nonlinearParams)
  let comp = s.complexity()
  inc functionsFit

  when EchoIntermediate:
    echo "> ", f, "    ", t.error, " ", log10(t.error)

  if t.error < expectedError:
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
    var e = newException(SufferingFromSuccessException, "Success!")
    e.pgeResult = PgeResult(
      text: text,
      error: t.error,
      logError: log10(t.error),
      complexity: comp,
      functionsFit: functionsFit,
      totalTime: getMonoTime() - startTime
    )
    raise e
  front.add text, t.error, comp
  mainQueue.add f, t.error, comp
  #echo " -> ", log10(t.error)

proc checkSuddenDrop() =
  let data = front.front

  var maxSlope = 0.0
  var maxI = 0
  echo ""
  echo "Slopes:"
  echo fmt"0{'\t'}{log10(data[0].error):.2f}{'\t'}----{'\t'}{data[0].text}"
  for i in 1..<data.len:
    let
      prevLogError = log10(data[i-1].error)
      thisLogError = log10(data[i].error)
      slope = (thisLogError - prevLogError) / 
        Number(data[i-1].complexity - data[i].complexity)
      c = data[i].complexity
    echo fmt"{c}{'\t'}{log10(data[i].error):.2f}{'\t'}{slope:.2f}{'\t'}{data[i].text}"
    if slope > maxSlope and data[i].complexity != 1:
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

    var e = newException(SufferingFromSuccessException, "Success!")
    e.pgeResult = PgeResult(
      text: d.text,
      error: d.error,
      logError: log10(d.error),
      complexity: d.complexity,
      functionsFit: functionsFit,
      totalTime: getMonoTime() - startTime
    )
    raise e
  else:
    let d = data[maxI]
    echo ""
    echo "Best:"
    echo fmt"Error: {d.error:.3e}"
    echo fmt"Log error: {log10(d.error):.2f}"
    echo fmt"Complexity: {d.complexity}"
    echo fmt"Functions evaluated: {functionsFit}"
    echo ""

proc calculatePge*(data: Data, addresses: seq[(string, uint16)],
    output: proc(s: string)) {.async.} =
  output "Start!"
  when RemoteEvaluation:
    initClient(addresses)
    output "Sucessfully connected!"

  startTime = getMonoTime()
  const basis = {
    # BasisFunction.Exp,
    # BasisFunction.Inverse,
    BasisFunction.IntPower,
    BasisFunction.Gaussian
    # BasisFunction.Sqrt
    # BasisFunction.Ln
    # BasisFunction.Sin,
    # BasisFunction.Cos
  }
  
  dataArray[false] = (data.trainX, data.trainY, 30)
  dataArray[true] = (data.approxX, data.approxY, 10)
  expectedError = data.errorBound
  if expectedError == 0:
    expectedError = 1e-7
  when RemoteEvaluation:
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

    for i in 0..<10000:
      let n = mainQueue.pop()
      # treeState[n.tree.id] = "#ffc300"
      output fmt"""{i}: {n.tree}
Error: {n.error:.3e}
Log error: {log10(n.error):.2f}
Complexity: {n.complexity}"""

      for tree in n.tree.generateFormulas(basis, VarCount):
        # tree.simplify().handleTree(data)
        approxFutures.add tree.simplify().approxTree()
        # file.writeLine(n.tree.id, ";", tree.id)
      if i < 5 or i mod 25 == 24:
        await all(approxFutures)
      else:
        asyncCheck all(approxFutures)

      const ApproxQueueCoefficient = 0.4
      let totalQueueSize = mainQueue.len + approxQueue.len
      let requiredMainQueue = ceil(totalQueueSize.float * ApproxQueueCoefficient).int
      let toTake = requiredMainQueue - mainQueue.len
      for _ in 0..<toTake:
        let node = approxQueue.pop()
        exactFutures.add node.tree.handleTree()
      if mainQueue.len == 0 or i mod 25 == 24:
        await all(exactFutures)
      else:
        asyncCheck all(exactFutures)

      echo ""
      
      if i mod 25 == 24:
        checkSuddenDrop()

when isMainModule:
  let data = generateData()
  # let data = readData("тест4.txt")
  # let file = openFileStream("data.txt", fmWrite)
  # for i in 0..<100:
  #   file.writeLine data.trainX[i][0], "\t", data.trainX[i][1]
  # file.writeLine ""
  # for i in 0..<100:
  #   if i > 0:
  #     file.write "\t"
  #   file.write $data.trainY[i]
  # file.close()

  when RemoteEvaluation:
    let params = commandLineParams()
    if params.len == 0:
      echo "Usage: pge ADDRESS:PORT ..."
    var addresses = newSeq[(string, uint16)]()
    for p in params:
      let s = p.split(':', 2)
      addresses.add (s[0], s[1].parseInt.uint16)
  else:
    let addresses = newSeq[(string, uint16)]()
  let f = calculatePge(data, addresses, proc(s: string) = echo s)
  while not f.finished:
    poll()
  if not (f.error of SufferingFromSuccessException):
    raise f.error
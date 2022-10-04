# This is just an example to get you started. A typical binary package
# uses this file as the main entry point of the application.

import nimprof 
import arraymancer
import expressions, formula, treegenerator, trie, ppq, varpro
import random, math, times, std/monotimes
import strformat

const VarCount = 2

proc generateData(): (Tensor[Number], Tensor[Number]) =
  const N = 100
  result[0] = newTensorUninit[Number](N, VarCount)
  result[1] = newTensorUninit[Number](N)
  for i in 0..<N:
    let x = rand(-3.0..3.0).Number
    let y = rand(-3.0..3.0).Number
    let f = x.pow(2) + y.pow(2) - 2 * x * y + 0.5 * x - 1.5 * y + 3
    #let f = exp(x) - 0.5 * exp(-0.5 * x)
    result[0][i,0] = x
    result[0][i,1] = y
    result[1][i] = f

var exprSet = newTrie[TotalKinds + VarCount]()
var queue: ParetoPriorityQueue
var front: ParetoFront
var startTime: Monotime
var functionsFit = 0

proc handleTree(e: Expression, data: (Tensor, Tensor)) =
  let s = e.serialized()
  if s in exprSet: return

  echo "> ", e

  let f = e.linearize()
  let t = f.fitParams(data[0], data[1])

  # echo t.linearParams
  # echo t.nonlinearParams
  let text = f.toString(t.linearParams, t.nonlinearParams)
  let comp = s.complexity()
  inc functionsFit

  if t.error < 10e-6:
    echo ""
    echo ""
    echo "!!! Found solution (very small error) !!!"
    echo text
    echo fmt"Error: {t.error:.3e}"
    echo fmt"Log error: {log10(t.error):.2f}"
    echo fmt"Complexity: {comp}"
    echo fmt"Functions evaluated: {functionsFit}"
    echo fmt"Total time: {getMonoTime() - startTime}"
    quit 0
  
  exprSet.add s
  front.add text, t.error, comp
  queue.add e, t.error, comp
  #echo " -> ", log10(t.error)

proc checkSuddenDrop() =
  let data = front.front

  var maxSlope = 0.0
  var maxI = 0
  for i in 1..<data.len:
    let
      prevLogError = log10(data[i-1].error)
      thisLogError = log10(data[i].error)
      slope = (thisLogError - prevLogError) / 
        Number(data[i-1].complexity - data[i].complexity)
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

when isMainModule:
  let data = generateData()

  startTime = getMonoTime()
  const basis = {
    BasisFunction.Exp,
    BasisFunction.IntPower
  }

  for i in 0..<VarCount:
    let e = nested(
      initBigExpr(Sum),
      initBigExpr(Product),
      initVariable(i)
    )
    e.handleTree(data)
  
  for i in 0..<1000:
    let n = queue.pop()
    echo i, ": ", n.tree
    echo fmt"Error: {n.error:.3e}"
    echo fmt"Log error: {log10(n.error):.2f}"
    echo fmt"Complexity: {n.complexity}"
    echo ""

    for tree in n.tree.generateTrees(basis, VarCount):
      tree.handleTree(data)
    
    if i mod 25 == 24:
      checkSuddenDrop()



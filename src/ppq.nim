import formula
from matrix import Number
import heapqueue

type
  PpqNode* = object
    tree*: LinearFormula
    error*: Number
    complexity*: int

func `<`(x, y: PpqNode): bool {.inline.} = x.error < y.error

type
  PpqStack = HeapQueue[PpqNode]
  ParetoPriorityQueue* = object
    stacks: seq[PpqStack]
    queue: seq[PpqNode]

type
  ParetoData = object
    text*: string
    error*: Number
    complexity*: int
  ParetoFront* = object
    front*: seq[ParetoData]

const
  MinComplexity = 0
  QueueSize = 4

func add*(ppq: var ParetoPriorityQueue,
    tree: LinearFormula,
    error: Number,
    complexity: int) =
  let n = PpqNode(tree: tree, error: error, complexity: complexity)
  let index = complexity - MinComplexity
  if ppq.stacks.len <= index:
    ppq.stacks.setLen(index+1)
  ppq.stacks[index].push n

func updateQueue(ppq: var ParetoPriorityQueue) =
  if ppq.queue.len != 0: return
  var prevError: Number = Inf
  for i in 0..<ppq.stacks.len:
    if ppq.stacks[i].len == 0: continue
    if ppq.stacks[i][0].error < prevError:
      let n = ppq.stacks[i].pop()
      prevError = n.error
      ppq.queue.add n
      if ppq.queue.len == QueueSize: break

func pop*(ppq: var ParetoPriorityQueue): PpqNode {.inline.} =
  ppq.updateQueue()
  ppq.queue.pop()

func add*(pf: var ParetoFront,
    text: string,
    error: Number,
    complexity: int) =
  let index = complexity - MinComplexity
  if pf.front.len <= index:
    pf.front.setLen(index+1)
  if pf.front[index].text == "" or error < pf.front[index].error:
    pf.front[index]  = ParetoData(
      text: text,
      error: error,
      complexity: complexity
    )
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
    size: int

type
  ParetoData = object
    text*: string
    error*: Number
    complexity*: int
  ParetoFront* = object
    data: seq[ParetoData]

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
  inc ppq.size

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
  dec ppq.size
  ppq.queue.pop()

func len*(ppq: ParetoPriorityQueue): int {.inline.} = ppq.size

func add*(pf: var ParetoFront,
    text: string,
    error: Number,
    complexity: int) =
  let index = complexity - MinComplexity
  if pf.data.len <= index:
    pf.data.setLen(index+1)
  if pf.data[index].text == "" or error < pf.data[index].error:
    pf.data[index]  = ParetoData(
      text: text,
      error: error,
      complexity: complexity
    )

func front*(pf: ParetoFront): seq[ParetoData] =
  # debugEcho pf.data
  result.add pf.data[0]
  for x in pf.data[1..^1]:
    if x.text == "": continue
    if x.error < result[^1].error:
      result.add x
  # quit()
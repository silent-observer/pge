import algorithm
import options

type 
  SmallSet*[T] = object
    data: seq[T]
    cmp: proc(a, b: T): int {.noSideEffect.}
  SmallSetIndex* = int

func initSmallSetWithNoCmp*[T](): SmallSet[T] =
  result.cmp = nil
func initSmallSet*[T](cmp: proc(a, b: T): int {.noSideEffect.}): SmallSet[T] =
  result.cmp = cmp

func `[]`*[T](s: SmallSet[T], i: SmallSetIndex): T {.inline.} = s.data[i]
func len*(s: SmallSet): int {.inline.} = s.data.len
func toSeq*[T](s: SmallSet[T]): seq[T] {.inline.} = s.data

func addIndex*[T](s: var SmallSet[T], x: T): SmallSetIndex =
  if s.data.len == 0:
    s.data.add x
    result = 0
  elif s.cmp == nil:
    result = s.data.len
    s.data.add x
  else:
    let i = s.data.lowerBound(x, s.cmp)
    if i == s.data.len or s.cmp(x, s.data[i]) != 0:
      s.data.insert(x, i)
    result = i
func add*[T](s: var SmallSet[T], x: T) {.inline.} =
  discard s.addIndex(x)
func add*[T](s: var SmallSet[T], x: Option[T]) {.inline.} =
  if x.isSome:
    discard s.addIndex(x.unsafeGet)

func delete*[T](s: var SmallSet[T], i: SmallSetIndex) {.inline.} =
  s.data.delete(i)
func clear*[T](s: var SmallSet[T]) {.inline.} =
  s.data.setLen 0

func find*[T](s: var SmallSet[T], x: T): SmallSetIndex {.inline.} =
  if s.cmp == nil:
    s.data.find(x)
  else:
    s.data.binarySearch(x, s.cmp)

func contains*[T](s: var SmallSet[T], x: T): bool {.inline.} =
  s.find(x) != -1

iterator items*[T](s: SmallSet[T]): T {.inline.} =
  for x in s.data:
    yield x
iterator pairs*[T](s: SmallSet[T]): (int, T) {.inline.} =
  for i, x in s.data:
    yield (i, x)
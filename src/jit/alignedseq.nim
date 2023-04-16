import algorithm

const Alignment = 32

type AlignedSeq*[T] = object
  data: seq[T]
  offset: int
  size: int

func realign[T](s: var AlignedSeq[T]) =
  if s.size == 0: return
  s.offset = 0
  while cast[uint64](addr s.data[s.offset]) mod Alignment != 0:
    inc s.offset

func realignWithCopy[T](s: var AlignedSeq[T]) =
  if s.size == 0: return
  let oldOffset = s.offset
  s.offset = 0
  while cast[uint64](addr s.data[s.offset]) mod Alignment != 0:
    inc s.offset
  if oldOffset != s.offset:
    moveMem(addr s.data[s.offset], addr s.data[oldOffset], s.size * sizeof(T))

func setLen*[T](s: var AlignedSeq[T], n: int) =
  const maxOffset = (Alignment div sizeof(T)) - 1

  s.data.setLen n+maxOffset
  s.size = n
  s.realignWithCopy()


func initAlignedSeq*[T](n: int): AlignedSeq[T] =
  const maxOffset = (Alignment div sizeof(T)) - 1

  result.data = newSeq[T](n+maxOffset)
  result.size = n
  result.realign()

func len*[T](s: AlignedSeq[T]) {.inline.} = s.size
template `[]`*[T](s: AlignedSeq[T], i: int): T = s.data[s.offset + i]
template `[]=`*[T](s: var AlignedSeq[T], i: int, val: T) = s.data[s.offset + i] = val

func add*[T](s: var AlignedSeq[T], val: T) =
  const maxOffset = (Alignment div sizeof(T)) - 1

  inc s.size
  s.data.setLen (s.size + maxOffset)
  s.realignWithCopy()
  s[s.size-1] = val

func add*[T](s: var AlignedSeq[T], vals: openArray[T]) =
  const maxOffset = (Alignment div sizeof(T)) - 1

  s.size += vals.len
  s.data.setLen (s.size + maxOffset)
  s.realignWithCopy()
  for i in 0..<vals.len:
    s[s.size - vals.len + i] = vals[i]

func fill*[T](s: var AlignedSeq[T], value: T) {.inline.} = s.data.fill(value)
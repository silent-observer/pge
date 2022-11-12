import ".."/[formula, variabledata, matrix]
import sequtils
from ../varpro import VarProResult
import asyncnet, asyncdispatch
import options

type
  TreeId* = byte
  Message* = object
    data*: seq[byte]
  Response* = object
    data*: seq[byte]

func encodeTree*(f: LinearFormula, id: TreeId, isApprox: bool): Message =
  result.data = @[id, byte(isApprox)] & f.serialized.mapIt(byte(it))

func encodeProblemSet*(isApprox: bool, x: VariableData, y: Vector, howMany=30): Message =
  let varCount = x.varCount
  let n = x.rows
  result.data = @[
    0'u8,
    byte(isApprox),
    byte(varCount),
    byte(howMany),
    byte(n and 0xFF),
    byte((n shr 8) and 0xFF),
    byte((n shr 16) and 0xFF),
    byte((n shr 24) and 0xFF)
  ]
  for i in 0..<n:
    for j in 0..<varCount:
      when Number is float64:
        result.data.add cast[array[8, byte]](x[i][j])
      when Number is float32:
        result.data.add cast[array[4, byte]](x[i][j])
  for i in 0..<n:
    when Number is float64:
      result.data.add cast[array[8, byte]](y[i])
    when Number is float32:
      result.data.add cast[array[4, byte]](y[i])

func encodeResponse*(id: TreeId, t: VarProResult): Response =
  let l = t.linearParams.len
  let nl = t.nonlinearParams.len
  result.data = @[
    byte(id),
    byte(l),
    byte(nl)
  ]
  for i in 0..<l:
    when Number is float64:
      result.data.add cast[array[8, byte]](t.linearParams[i])
    when Number is float32:
      result.data.add cast[array[4, byte]](t.linearParams[i])
  for i in 0..<nl:
    when Number is float64:
      result.data.add cast[array[8, byte]](t.nonlinearParams[i])
    when Number is float32:
      result.data.add cast[array[4, byte]](t.nonlinearParams[i])
  when Number is float64:
    result.data.add cast[array[8, byte]](t.error)
  when Number is float32:
    result.data.add cast[array[4, byte]](t.error)

func isTree*(m: Message): bool {.inline.} = m.data[0] > 0'u8

func decodeTree*(m: Message): (LinearFormula, TreeId, bool) =
  assert m.isTree
  result[1] = m.data[0]
  result[2] = m.data[1].bool
  result[0] = m.data[2..^1].mapIt(int(it)).deserializeFormula()

func decodeProblemSet*(m: Message): (VariableData, Vector, int, bool) =
  assert not m.isTree
  let varCount = m.data[2].int
  result[2] = m.data[3].int
  result[3] = m.data[1].bool
  let n = (
    (m.data[4].int) or
    (m.data[5].int shl 8) or
    (m.data[6].int shl 16) or
    (m.data[7].int shl 24)
  )
  result[0] = initVariableData(varCount)
  var index = 8
  for i in 0..<n:
    var row = newSeq[Number](varCount)
    when Number is float64:
      copyMem(addr row[0], unsafeAddr m.data[index], 8 * varCount)
      index += 8 * varCount
    when Number is float32:
      copyMem(addr row[0], unsafeAddr m.data[index], 4 * varCount)
      index += 4 * varCount
    result[0].add row

  var y = newSeq[Number](n)
  when Number is float64:
    copyMem(addr y[0], unsafeAddr m.data[index], 8 * n)
  when Number is float32:
    copyMem(addr y[0], unsafeAddr m.data[index], 4 * n)
  result[1] = vector(y)

func decodeResponse*(m: Response): (VarProResult, TreeId) =
  result[1] = m.data[0]
  let l = m.data[1].int
  let nl = m.data[2].int
  var index = 3
  result[0].linearParams = vector(l)
  result[0].nonlinearParams = vector(nl)
  when Number is float64:
    copyMem(result[0].linearParams.getAddr(0), unsafeAddr m.data[index], 8 * l)
    index += 8 * l
    copyMem(result[0].nonlinearParams.getAddr(0), unsafeAddr m.data[index], 8 * nl)
    index += 8 * nl
    copyMem(addr result[0].error, unsafeAddr m.data[index], 8)
  when Number is float32:
    copyMem(result[0].linearParams.getAddr(0), unsafeAddr m.data[index], 4 * l)
    index += 4 * l
    copyMem(result[0].nonlinearParams.getAddr(0), unsafeAddr m.data[index], 4 * nl)
    index += 4 * nl
    copyMem(addr result[0].error, unsafeAddr m.data[index], 4)

proc send*[T: Message|Response](s: AsyncSocket, m: T) {.async.} =
  let l = uint16(m.data.len)
  var data = @[0'u8, 0] & m.data
  copyMem(addr data[0], unsafeAddr l, 2)
  await s.send(unsafeAddr data[0], data.len)
  # echo "Sent ", l, " bytes!"
  # echo "Sent ", m.data, " bytes!"

proc recvMessage*(s: AsyncSocket): Future[Option[Message]] {.async.} =
  let dataLenArr = await s.recv(2)
  if dataLenArr.len == 0:
    return none(Message)
  var dataLen: uint16
  copyMem(addr dataLen, unsafeAddr dataLenArr[0], 2)
  # echo "Received length!"
  let data = await s.recv(dataLen.int)
  if data.len != dataLen.int:
    return none(Message)
  # echo "Received ", dataLen, " bytes!"
  result = Message(data: cast[seq[byte]](data)).some

proc recvResponse*(s: AsyncSocket): Future[Option[Response]] {.async.} =
  let dataLenArr = await s.recv(2)
  if dataLenArr.len == 0:
    return none(Response)
  var dataLen: uint16
  copyMem(addr dataLen, unsafeAddr dataLenArr[0], 2)
  let data = await s.recv(dataLen.int)
  if data.len != dataLen.int:
    return none(Response)
  # echo "Received ", dataLen, " bytes!"
  result = Response(data: cast[seq[byte]](data)).some
  # echo "Received ", cast[seq[byte]](data), " bytes!"
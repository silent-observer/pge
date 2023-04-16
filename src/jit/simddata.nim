import ../matrix
import ../variabledata
from nimblas import copy
import alignedseq

type 
  SimdData* = object
    varCount*, rows*, simdWidth*: int
    data*: ref AlignedSeq[Number]
  SimdSlice* = object
    varCount*, simdWidth*: int
    data* {.cursor.}: ref AlignedSeq[Number]
    row*: int
  SimdVector* = object
    size*, simdWidth*: int
    data*: AlignedSeq[Number]

func toSimd*(varData: VariableData, simdWidth: int): SimdData =
  assert simdWidth in {1, 2, 4}
  result.varCount = varData.varCount
  result.rows = varData.rows
  result.simdWidth = simdWidth

  new(result.data)
  let combinedRows = (result.rows + (simdWidth - 1)) div simdWidth
  result.data[] = initAlignedSeq[Number](combinedRows * simdWidth * varData.varCount)

  for i in 0..<combinedRows:
    for j in 0..<simdWidth:
      let index = i * simdWidth + j
      if index >= varData.rows:
        break
      copy(
        result.varCount,
        addr varData.data[index * varData.varCount],
        1,
        addr result.data[][i * simdWidth * varData.varCount + j],
        simdWidth
      )

func `[]`*(d: SimdData, i: int): SimdSlice {.inline.} =
  assert i mod d.simdWidth == 0
  result.data = d.data
  result.row = i
  result.simdWidth = d.simdWidth
  result.varCount = d.varCount
func getAddr*(d: SimdSlice): ptr Number {.inline.} =
  assert d.row mod d.simdWidth == 0
  unsafeAddr d.data[][(d.row div d.simdWidth) * d.simdWidth * d.varCount]


func initSimdVector*(n: int, simdWidth: int): SimdVector =
  result.size = n
  result.simdWidth = simdWidth
  result.data = initAlignedSeq[Number](n * simdWidth)

func toSimd*(vec: Vector, simdWidth: int): SimdVector =
  result = initSimdVector(vec.len, simdWidth)
  for i in 0..<vec.len:
    let x = vec.data[i]
    for j in 0..<simdWidth:
      result.data[i*simdWidth + j] = x

func getAddr*(vec: SimdVector, offset: int): ptr Number {.inline.} =
  if offset >= vec.size:
    nil
  else:
    unsafeAddr vec.data[vec.simdWidth * offset]

func setRows*(m: var Matrix, rows: SimdVector, i: int) {.inline.} =
  assert(m.cols == rows.size, "Vector dimension must match")
  for j in 0..<rows.simdWidth:
    if i+j < m.rows:
      copy(m.cols, unsafeAddr rows.data[j], rows.simdWidth, addr m[i+j, 0], m.rows)

# when isMainModule:
#   var vData = initVariableData(3)
#   vData.add [10.0, 20.0, 30.0]
#   vData.add [40.0, 50.0, 60.0]
#   vData.add [70.0, 80.0, 90.0]
#   vData.add [100.0, 110.0, 120.0]
#   vData.add [130.0, 140.0, 150.0]
#   vData.add [160.0, 170.0, 180.0]
#   vData.add [190.0, 200.0, 210.0]
#   vData.add [220.0, 230.0, 240.0]
#   vData.add [250.0, 260.0, 270.0]
#   echo vData.data[]

#   let simd2 = vData.toSimd(2)
#   echo simd2.data[]
#   let simd4 = vData.toSimd(4)
#   echo simd4.data[]
from matrix import Number

type 
  VariableData* = object
    varCount*, rows*: int
    data*: ref seq[Number]
  VariableSlice* = object
    varCount*: int
    data*: ref seq[Number]
    row*: int

func initVariableData*(varCount: int): VariableData =
  result.varCount = varCount
  result.rows = 0
  new(result.data)

func add*(d: var VariableData, row: seq[Number]) =
  assert row.len == d.varCount, "Row length doesn't match!"
  d.data[].add row
  inc d.rows

func toVariableData*(data: seq[seq[Number]]): VariableData =
  result = initVariableData(data[0].len)
  for row in data:
    result.add row

func `[]`*(d: VariableData, i: int): VariableSlice {.inline.} =
  VariableSlice(varCount: d.varCount, data: d.data, row: i)
func `[]`*(slice: VariableSlice, i: int): Number {.inline.} =
  slice.data[slice.row * slice.varCount + i]
func getAddr*(slice: VariableSlice): ptr Number {.inline.} =
  unsafeAddr slice.data[slice.row * slice.varCount]
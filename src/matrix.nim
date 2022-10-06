import nimblas
import nimlapack
from algorithm import fill

type
  Number* = float64
  Vector* = object
    data*: seq[Number]
  Matrix* = object
    rows*, cols*: int
    data*: seq[Number]
  SymmetricMatrix* = distinct Matrix
  LowerTriangularMatrix* = distinct Matrix
  DiagonalMatrix* = distinct Vector

func vector*(s: openArray[Number]): Vector {.inline.} =
  Vector(data: @s)
func vector*(n: int): Vector {.inline.} =
  Vector(data: newSeq[Number](n))

func `$`*(v: Vector): string {.inline.} = 
  "Vector<" & $v.data.len & ">(" & $v.data & ")"
func `$`*(m: Matrix): string {.inline.} = 
  "Matrix<" & $m.rows & ", " & $m.cols & ">(" & $m.data & ")"
func `$`*(m: SymmetricMatrix): string {.inline.} = 
  "SymmetricMatrix<" & $m.Matrix.rows & ", " & $m.Matrix.cols & ">"
func `$`*(m: LowerTriangularMatrix): string {.inline.} = 
  "LowerTriangularMatrix<" & $m.Matrix.rows & ", " & $m.Matrix.cols & ">"
func `$`*(m: DiagonalMatrix): string {.inline.} = 
  "DiagonalMatrix<" & $m.Vector.data.len & ", " & $m.Vector.data.len & 
    ">(" & $m.Vector.data & ")"

proc d(v: var Vector): ptr Number {.inline.} = addr v.data[0]
proc d(v: Vector): ptr Number {.inline.} = unsafeAddr v.data[0]
proc d(m: var Matrix): ptr Number {.inline.} = addr m.data[0]
proc d(m: Matrix): ptr Number {.inline.} = unsafeAddr m.data[0]

proc `[]`*(v: Vector, i: int): Number {.inline.} = v.data[i]
proc `[]`*(v: var Vector, i: int): var Number {.inline.} = v.data[i]
proc `[]=`*(v: var Vector, i: int, x: Number) {.inline.} = v.data[i] = x
proc `[]`*(m: Matrix, i,j: int): Number {.inline.} = m.data[i*m.cols + j]
proc `[]`*(m: var Matrix, i,j: int): var Number {.inline.} = m.data[i*m.cols + j]
proc `[]=`*(m: var Matrix, i,j: int, x: Number) {.inline.} = m.data[i*m.cols + j] = x

func matrix*(rows, cols: int, data: openArray[Number]): Matrix {.inline.} =
  Matrix(rows: rows, cols: cols, data: @data)
func matrix*(rows, cols: int): Matrix {.inline.} =
  Matrix(rows: rows, cols: cols, data: newSeq[Number](rows*cols))
func matrix*(r: seq[seq[Number]]): Matrix {.inline.} =
  let rows = r.len
  let cols = r[0].len
  var data = newSeqOfCap[Number](rows*cols)
  for row in r:
    data.add row
  Matrix(rows: rows, cols: cols, data: data)

func len*(v: Vector): int {.inline.} = v.data.len

proc `+=`*(a: var Vector, b: Vector) {.inline.} =
  assert(a.len == b.len, "Vector dimensions must be the same")
  axpy(a.len, 1.0, b.d, 1, a.d, 1)

proc `-=`*(a: var Vector, b: Vector) {.inline.} =
  assert(a.len == b.len, "Vector dimensions must be the same")
  axpy(a.len, -1.0, b.d, 1, a.d, 1)
proc `+`*(a, b: Vector): Vector {.inline.} =
  result = a
  result += b
proc `-`*(a, b: Vector): Vector {.inline.} =
  result = a
  result -= b

proc `+=`*(a: var Matrix, b: Matrix) {.inline.} =
  assert(a.rows == b.rows and a.cols == b.cols, "Matrix dimensions must be the same")
  axpy(a.rows * a.cols, 1.0, b.d, 1, a.d, 1)

proc `-=`*(a: var Matrix, b: Matrix) {.inline.} =
  assert(a.rows == b.rows and a.cols == b.cols, "Matrix dimensions must be the same")
  axpy(a.rows * a.cols, -1.0, b.d, 1, a.d, 1)

proc `+`*(a, b: Matrix): Matrix {.inline.} =
  result = a
  result += b
proc `-`*(a, b: Matrix): Matrix {.inline.} =
  result = a
  result -= b

proc `*=`*(a: var Vector, s: Number) {.inline.} =
  scal(a.len, s, a.d, 1)
proc `*`*(a: Vector, s: Number): Vector {.inline.} =
  result = a
  result *= s
proc `*`*(s: Number, a: Vector): Vector {.inline.} = a * s
proc `-`*(a: Vector): Vector {.inline.} = -1.0 * a

proc `*=`*(a: var Matrix, s: Number) {.inline.} =
  scal(a.rows * a.cols, s, a.d, 1)
proc `*`*(a: Matrix, s: Number): Matrix {.inline.} =
  result = a
  result *= s
proc `*`*(s: Number, a: Matrix): Matrix {.inline.} = a * s
proc `-`*(a: Matrix): Matrix {.inline.} = -1.0 * a

proc mulAddInPlace*(a: var Vector, s: Number, b: Vector) {.inline.} =
  # a += s * b
  assert(a.len == b.len, "Vector dimensions must be the same")
  axpy(a.len, s, b.d, 1, a.d, 1)
proc mulAdd*(a: Vector, s: Number, b: Vector): Vector {.inline.} =
  # a + s * b
  result = a
  result.mulAddInPlace(s, b)

template rewriteMulAdd*{a + `*`(s, b)}(a, b: Vector, s: Number): Vector =
  mulAdd(a, s, b)

template rewriteMulAdd*{a += `*`(s, b)}(a: var Vector, b: Vector, s: Number) =
  mulAddInPlace(a, s, b)

proc dot*(a, b: Vector): Number {.inline.} =
  assert(a.len == b.len, "Vector dimensions must be the same")
  dot(a.len, a.d, 1, b.d, 1)

proc norm*(a: Vector): Number {.inline.} =
  nrm2(a.len, a.d, 1)
proc absMax*(a: Vector): Number {.inline.} =
  let index = iamax(a.len, a.d, 1)
  a.data[index].abs()

proc `*`* (m: Matrix, v: Vector): Vector {.inline.} =
  assert(v.len == m.cols, "Vector dimension must match")
  result = vector(m.rows)
  gemv(rowMajor, noTranspose, m.rows, m.cols, 1.0, m.d,
    m.cols, v.d, 1, 0.0, result.d, 1)
proc `^*`* (m: Matrix, v: Vector): Vector {.inline.} =
  assert(v.len == m.rows, "Vector dimension must match")
  result = vector(m.cols)
  gemv(rowMajor, transpose, m.rows, m.cols, 1.0, m.d,
    m.cols, v.d, 1, 0.0, result.d, 1)

proc `*`* (m: SymmetricMatrix, v: Vector): Vector {.inline.} =
  assert(m.Matrix.rows == m.Matrix.cols, "Matrix must be square")
  assert(v.len == m.Matrix.cols, "Vector dimension must match")
  result = vector(m.Matrix.rows)
  symv(rowMajor, upper, m.Matrix.rows, 1.0, m.Matrix.d,
    m.Matrix.cols, v.d, 1, 0.0, result.d, 1)

proc `*`* (m: DiagonalMatrix, v: Vector): Vector {.inline.} =
  assert(v.len == m.Vector.len, "Vector dimension must match")
  result = vector(v.len)
  for i in 0..<v.len:
    result[i] = m.Vector[i] * v[i]

proc setRow*(m: var Matrix, row: Vector, i: int) {.inline.} =
  assert(m.cols == row.len, "Vector dimension must match")
  m.data[i*m.cols..<(i+1)*m.cols] = row.data

proc t*(m: Matrix): Matrix {.inline.} =
  result = matrix(m.cols, m.rows)
  for i in 0..<m.rows:
    for j in 0..<m.cols:
      result[j,i] = m[i,j]

var scratchSpace: seq[Number]

proc svd*(a: Matrix): tuple[u: Matrix, sigma: DiagonalMatrix, vt: Matrix] =
  when Number is float64:
    template gesvd(args: varargs[untyped]): untyped = dgesvd(args)
  elif Number is float32:
    template gesvd(args: varargs[untyped]): untyped = sgesvd(args)
  else: {.error.}


  let
    aTrans = a.t
    m = a.rows.cint
    n = a.cols.cint
    k = min(m, n).cint
  var 
    workSize: Number
    lwork = cint(-1)
    info: int32
  result.u = matrix(k, m)
  result.sigma = vector(k).DiagonalMatrix
  result.vt = matrix(n, k)

  gesvd("S", "S", unsafeAddr m, unsafeAddr n, aTrans.d, unsafeAddr m, # lda
    result.sigma.Vector.d,
    result.u.d, unsafeAddr m, # ldu
    result.vt.d, unsafeAddr k, #ldvt
    addr workSize, addr lwork, addr info
    )
  lwork = workSize.int32
  scratchSpace.setLen workSize.int32

  gesvd("S", "S", unsafeAddr m, unsafeAddr n, aTrans.d, unsafeAddr m, # lda
    result.sigma.Vector.d,
    result.u.d, unsafeAddr m, # ldu
    result.vt.d, unsafeAddr k, #ldvt
    addr scratchSpace[0], addr lwork, addr info
    )

  if info > 0:
    result.sigma.Vector.data.fill(Inf)
  else:
    result.u = result.u.t
    result.vt = result.vt.t

proc choleskyDecompose*(a: SymmetricMatrix): LowerTriangularMatrix =
  when Number is float64:
    template potrf(args: varargs[untyped]): untyped = dpotrf(args)
  elif Number is float32:
    template potrf(args: varargs[untyped]): untyped = spotrf(args)
  else: {.error.}

  result = a.Matrix.LowerTriangularMatrix
  let n = a.Matrix.rows.cint
  var info: cint
  potrf("L", unsafeAddr n, result.Matrix.d, unsafeAddr n, addr info)

proc choleskySolve*(a: LowerTriangularMatrix, v: Vector): Vector =
  assert(v.len == a.Matrix.rows, "Vector dimension must match")
  result = v

  trsv(rowMajor, lower, transpose, nonUnit, result.len, a.Matrix.d, v.len, result.d, 1)
  trsv(rowMajor, lower, noTranspose, nonUnit, result.len, a.Matrix.d, v.len, result.d, 1)

proc gramMatrix*(a: Matrix): SymmetricMatrix =
  result = matrix(a.cols, a.cols).SymmetricMatrix
  syrk(rowMajor, upper, transpose, a.cols, a.rows, 1.0, a.d, a.cols, 
    0.0, result.Matrix.d, a.cols)

proc reverseGramMatrix*(a: Matrix): SymmetricMatrix =
  result = matrix(a.rows, a.rows).SymmetricMatrix
  syrk(rowMajor, upper, noTranspose, a.rows, a.cols, 1.0, a.d, a.cols, 
    0.0, result.Matrix.d, a.rows)

proc gemm(a, b: Matrix, transA, transB: TransposeType): Matrix =
  let (m, k1) = if transA == noTranspose: (a.rows, a.cols) else: (a.cols, a.rows)
  let (k2, n) = if transB == noTranspose: (b.rows, b.cols) else: (b.cols, b.rows)
  assert(k1 == k2, "Matrix dimensions must match")
  result = matrix(m, n)
  gemm(rowMajor, transA, transB, m, n, k1, 1.0, a.d, a.cols, b.d, b.cols,
    0.0, result.d, result.cols)

proc `*`*(a, b: Matrix): Matrix {.inline.} =
  gemm(a, b, noTranspose, noTranspose)
proc `*^`*(a, b: Matrix): Matrix {.inline.} =
  gemm(a, b, noTranspose, transpose)
proc `^*`*(a, b: Matrix): Matrix {.inline.} =
  gemm(a, b, transpose, noTranspose)
proc `^*^`*(a, b: Matrix): Matrix {.inline.} =
  gemm(a, b, transpose, transpose)
proc fromSymmetric*(a: SymmetricMatrix): Matrix {.inline.} =
  result = a.Matrix
  for i in 1..<result.rows:
    for j in 0..<i:
      result[i,j] = result[j,i]

proc `*=`*(a: var Matrix, b: DiagonalMatrix) {.inline.} =
  assert(a.cols == b.Vector.len, "Matrix dimensions must match")
  for k in 0..<a.cols:
    scal(a.rows, b.Vector[k], addr a[0, k], a.cols)
proc `*`*(a: Matrix, b: DiagonalMatrix): Matrix {.inline.} =
  result = a
  result *= b

proc `*`*(a: DiagonalMatrix, b: Matrix): Matrix=
  assert(a.Vector.len == b.rows, "Matrix dimensions must match")
  result = b
  for i in 0..<b.rows:
    scal(b.cols, a.Vector[i], addr result[i, 0], 1)

template rewriteGram*{`^*`(a, a)}(a: Matrix): Matrix =
  gramMatrix(a).fromSymmetric()
template rewriteReverseGram*{`^*`(a, a)}(a: Matrix): Matrix =
  reverseGramMatrix(a).fromSymmetric()

when isMainModule:
  let a = matrix(@[
    @[1.0, 2, 3],
    @[4.0, 5, 6]
  ])
  let decomp = a.svd()
  echo a
  echo decomp.u
  echo decomp.sigma
  echo decomp.vt
  echo decomp.u * decomp.sigma * decomp.vt
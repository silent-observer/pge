import ir, optimizer, registerallocator, assembler
import ../expressions, ../variabledata, ../matrix
import simddata
import posix
from strutils import toHex
import lrucache
import alignedseq

type
  JitProcedure = proc(vars, params, derivs, result: ptr float64, data: pointer) {.cdecl.}
  JitProgramObj = object
    codePage: pointer
    length: int
    evalProc, evalAllProc: JitProcedure
    data: AlignedSeq[byte]
  JitProgram* = ref JitProgramObj

const AllowSimd* = true

let simdCapability = if AllowSimd: determineSimdCapability() else: NoCapability
let simdWidth* = case simdCapability:
  of NoCapability: 1
  of Avx: 4

proc `=destroy`*(j: var JitProgramObj) =
  discard munmap(j.codePage, j.length)
proc `=copy`*(dest: var JitProgramObj, src: JitProgramObj) {.error.}
proc `=sink`*(dest: var JitProgramObj, src: JitProgramObj) =
  `=destroy`(dest)
  wasMoved(dest)
  dest.codePage = src.codePage
  dest.length = src.length
  dest.evalProc = src.evalProc
  dest.data = src.data

proc compileUncached(e: Expression): JitProgram =
  new(result)
  let 
    p1 = e.convertToIr()
    p2 = p1.optimize()
    p3Full = p2.allocate(simdCapability)
    p3OnlyEval = p2.allocateOnlyEval(simdCapability)

  # echo e
  # echo p1
  # echo p2
  # echo p3Full

  let
    codeFull = p3Full.assemble(simdCapability)
    codeOnlyEval = p3OnlyEval.assemble(simdCapability)
    totalCode = codeOnlyEval.prog & codeFull.prog

  # for b in totalCode:
  #   stdout.write("" & b.toHex() & " ")
  # echo ""

  result.length = totalCode.len
  result.codePage = mmap(
      nil,
      result.length,
      PROT_EXEC or PROT_READ or PROT_WRITE,
      MAP_SHARED or MAP_ANONYMOUS,
      -1, 0
    )
  copyMem(result.codePage, unsafeAddr totalCode[0], totalCode.len)
  result.evalProc = cast[JitProcedure](result.codePage)
  result.evalAllProc = cast[JitProcedure](
    cast[int](result.codePage) + codeOnlyEval.prog.len
  )

  result.data = codeFull.data

var cache = newLruCache[SerializedExpr, JitProgram](256)
proc compile*(e: Expression): JitProgram =
  let s = e.serialized()
  if s in cache:
    result = cache[s]
  else:
    result = e.compileUncached()
    cache[s] = result

when AllowSimd:
  type
    JitVariableInput* = SimdSlice
    JitVectorInput* = SimdVector
    JitOutput* = SimdVector
else:
  type
    JitVariableInput* = VariableSlice
    JitVectorInput* = Vector
    JitOutput* = Number

proc evalAll*(j: JitProgram,
    vars: JitVariableInput,
    params: JitVectorInput,
    paramIndex: int,
    derivs: var JitVectorInput,
    result: var JitOutput) =
  when AllowSimd:
    let resultAddr = result.getAddr(0)
  else:
    let resultAddr = addr result
  j.evalAllProc(
    getAddr vars,
    params.getAddr(paramIndex),
    derivs.getAddr(paramIndex),
    resultAddr,
    unsafeAddr j.data[0]
  )
proc eval*(j: JitProgram,
    vars: JitVariableInput,
    paramIndex: int,
    params: JitVectorInput,
    result: var JitOutput) =
  when AllowSimd:
    let resultAddr = result.getAddr(0)
  else:
    let resultAddr = addr result
  j.evalProc(
    getAddr vars,
    params.getAddr(paramIndex),
    nil,
    resultAddr,
    unsafeAddr j.data[0]
  )

when isMainModule:
  # let e = initBigExpr(Sum).withChildren(
  #   initBigExpr(Product).withChildren(
  #     initVariable(0)
  #   ),
  #   initBigExpr(Product).withChildren(
  #     initVariable(1),
  #     initUnaryExpr(Inverse).withChildren(
  #       initBigExpr(Sum).withChildren(
  #         initBigExpr(Product, constDisabled=true).withChildren(
  #           initIntPower(2).withChildren(initVariable(0))
  #         ),
  #         initBigExpr(Product).withChildren(
  #           initIntPower(2).withChildren(initVariable(1))
  #         )
  #       )
  #     )
  #   )
  # )
  let e = initUnaryExpr(Sin).nested(
    initBigExpr(Sum),
    initBigExpr(Product),
    initVariable(0)
  )
  # let e = initBigExpr(Sum).withChildren(
  #   initBigExpr(Product).nested(
  #     initIntPower(10),
  #     initVariable(0)
  #   ),
  #   initBigExpr(Product).nested(
  #     initIntPower(9),
  #     initVariable(0)
  #   ),
  #   initBigExpr(Product).nested(
  #     initIntPower(5),
  #     initVariable(0)
  #   ),
  #   initBigExpr(Product).nested(
  #     initIntPower(3),
  #     initVariable(0)
  #   ),
  #   initBigExpr(Product).nested(
  #     initIntPower(2),
  #     initVariable(0)
  #   ),
  #   initBigExpr(Product).nested(
  #     initVariable(0)
  #   )
  # )
  
  echo e
  echo ""
  let j = e.compile()
  let vars = toVariableData(@[
    #@[1.5, 2.0]
    # @[0.0, 1.0],
    # @[2.0, 2.0],
    # @[-1.0, 3.0],
    # @[-2.0, -1.0],
    @[0.0],
    @[1.0],
    @[2.0],
    @[3.0]
  ]).toSimd(simdWidth)
  echo vars.data[]
  let params = vector(@[0.0, 1.570796]).toSimd(simdWidth)
    #vector(@[1.0, 1.0, 2.0, -0.5, -1.0, 1.0, 2.0]).toSimd(simdWidth)
  var derivs = vector(2).toSimd(simdWidth)
  echo params.data
  echo derivs.data
  var r = initSimdVector(1, simdWidth)
  j.evalAll(vars[0], params, 0, derivs, r)
  echo r
  echo derivs
import ir, optimizer, registerallocator, assembler
import ../expressions, ../variabledata, ../matrix
import posix
from strutils import toHex
import lrucache

type
  JitProcedure = proc(vars, params, derivs, result: ptr float64, data: pointer) {.cdecl.}
  JitProgramObj = object
    codePage: pointer
    length: int
    evalProc, evalAllProc: JitProcedure
    data: seq[byte]
    constsPtrShift: int
  JitProgram* = ref JitProgramObj

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
    p3Full = p2.allocate()
    p3OnlyEval = p2.allocateOnlyEval()
    codeFull = p3Full.assemble()
    codeOnlyEval = p3OnlyEval.assemble()
    totalCode = codeOnlyEval.prog & codeFull.prog

  # echo e
  # echo p1
  # echo p2
  # echo p3Full

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

  result.data = newSeqOfCap[byte](codeFull.data.len + 16)
  result.data.add codeFull.data
  result.constsPtrShift = 0
  while (cast[int](addr result.data[result.constsPtrShift]) and 0xF) != 0:
    result.data.insert(0'u8, 0)
    inc result.constsPtrShift

var cache = newLruCache[SerializedExpr, JitProgram](256)
proc compile*(e: Expression): JitProgram =
  let s = e.serialized()
  if s in cache:
    result = cache[s]
  else:
    result = e.compileUncached()
    cache[s] = result

proc evalAll*(j: JitProgram,
    vars: VariableSlice,
    params: Vector,
    paramIndex: int,
    derivs: var Vector): Number =
  j.evalAllProc(
    getAddr vars,
    params.getAddr(paramIndex),
    derivs.getAddr(paramIndex),
    addr result,
    unsafeAddr j.data[j.constsPtrShift]
  )
proc eval*(j: JitProgram,
    vars: VariableSlice,
    paramIndex: int,
    params: Vector): Number =
  j.evalProc(
    getAddr vars,
    params.getAddr(paramIndex),
    nil,
    addr result,
    unsafeAddr j.data[j.constsPtrShift]
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
  let e = initBigExpr(Sum).withChildren(
    initBigExpr(Product).nested(
      initIntPower(10),
      initVariable(0)
    ),
    initBigExpr(Product).nested(
      initIntPower(9),
      initVariable(0)
    ),
    initBigExpr(Product).nested(
      initIntPower(5),
      initVariable(0)
    ),
    initBigExpr(Product).nested(
      initIntPower(3),
      initVariable(0)
    ),
    initBigExpr(Product).nested(
      initIntPower(2),
      initVariable(0)
    ),
    initBigExpr(Product).nested(
      initVariable(0)
    )
  )
  
  echo e
  echo ""
  let j = e.compile()
  let vars = toVariableData(@[
    #@[1.5, 2.0]
    @[2.0]
  ])
  let params = #vector(@[1.0, -1.0, 1.0, 0.1, 1.0])
    vector(@[1.0, 1.0, 2.0, -0.5, -1.0, 1.0, 2.0])
  var derivs = vector(7)
  let r = j.evalAll(vars[0], params, 0, derivs)
  echo r
  echo derivs
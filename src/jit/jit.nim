import ir, optimizer, registerallocator, assembler
import ../expressions, ../variabledata, ../matrix
import posix
from strutils import toHex

type
  JitProcedure = proc(vars, params, derivs, result, consts: ptr float64) {.cdecl.}
  JitProgramObj = object
    codePage: pointer
    length: int
    evalProc, evalAllProc: JitProcedure
    consts: seq[float64]
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
  dest.consts = src.consts

proc compile*(e: Expression): JitProgram =
  new(result)
  let 
    p1 = e.convertToIr()
    p2 = p1.optimize()
    p3Full = p2.allocate()
    p3OnlyEval = p2.allocateOnlyEval()
    codeFull = p3Full.assemble()
    codeOnlyEval = p3OnlyEval.assemble()
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

  result.consts = newSeqOfCap[float64](codeFull.consts.len + 1)
  result.consts.add codeFull.consts
  if (cast[int](addr result.consts[0]) and 0xF) != 0:
    result.consts.insert(0.0, 0)
    result.constsPtrShift = 1
  else:
    result.constsPtrShift = 0
  

proc evalAll*(j: JitProgram,
    vars: VariableSlice,
    params: Vector,
    derivs: var Vector): Number =
  j.evalAllProc(
    getAddr vars,
    getAddr params,
    getAddr derivs,
    addr result,
    unsafeAddr j.consts[j.constsPtrShift]
  )
proc eval*(j: JitProgram,
    vars: VariableSlice,
    params: Vector): Number =
  j.evalProc(
    getAddr vars,
    getAddr params,
    nil,
    addr result,
    unsafeAddr j.consts[j.constsPtrShift]
  )

when isMainModule:
  let e = initBigExpr(Sum).withChildren(
    initBigExpr(Product).withChildren(
      initVariable(0)
    ),
    initBigExpr(Product).withChildren(
      initVariable(1),
      initUnaryExpr(Inverse).withChildren(
        initBigExpr(Sum).withChildren(
          initBigExpr(Product, constDisabled=true).withChildren(
            initIntPower(2).withChildren(initVariable(0))
          ),
          initBigExpr(Product).withChildren(
            initIntPower(2).withChildren(initVariable(1))
          )
        )
      )
    )
  )
  
  let j = e.compile()
  let vars = toVariableData(@[
    @[1.5, 2.0]
  ])
  let params = vector(@[1.0, -1.0, 1.0, 0.1, 1.0])
  var derivs = vector(5)
  let r = j.evalAll(vars[0], params, derivs)
  echo r
  echo derivs
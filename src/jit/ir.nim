import ../expressions
from strutils import join
from math import sqrt, PI
import cpuwhat

type
  SimdCapability* = enum
    NoCapability
    Avx
  CommandKind* = enum
    ckMov
    ckNop
    ckAdd
    ckSub
    ckMul
    ckDiv
    ckInv
    ckFuncCall
    ckIntPower
    ckPush,
    ckPop
  CommandArgumentKind* = enum
    cakVariable
    cakParameter
    cakIntermediate
    cakZero
    cakOne
    cakMinusOne
    cakConst
    cakRegister
    cakMemory
    cakDerivative
    cakResult
  CommandArgument* = object
    case kind*: CommandArgumentKind
    of cakVariable,
        cakParameter,
        cakIntermediate,
        cakRegister,
        cakMemory,
        cakDerivative:
      id*: int
    of cakConst:
      c*: float64
    else:
      discard
  Command* = object
    case kind*: CommandKind
    of ckFuncCall:
      funcName*: string
    of ckIntPower:
      power*: int
    else:
      discard
    args*: seq[CommandArgument]
    result*: CommandArgument
  Program* = object
    forward*, backward*: seq[Command]
    forwardResult*: CommandArgument
    derivatives*: seq[CommandArgument]
    memorySize*: int
  IrConverter = object
    paramIndex: int
    nextIntermediate: int
    derivatives: seq[CommandArgument]

func `==`*(a, b: CommandArgument): bool =
  if a.kind != b.kind: return false
  case a.kind:
  of cakConst: a.c == b.c
  of cakVariable,
      cakParameter,
      cakIntermediate,
      cakRegister,
      cakMemory,
      cakDerivative:
    a.id == b.id
  else: true

func determineSimdCapability*(): SimdCapability =
  {.cast(noSideEffect).}:
    if hasAVX():
      Avx
    else:
      NoCapability

func nextInter(c: var IrConverter): CommandArgument {.inline.} =
  result = CommandArgument(kind: cakIntermediate, id: c.nextIntermediate)
  inc c.nextIntermediate

func variable(i: int): CommandArgument {.inline.} =
  CommandArgument(kind: cakVariable, id: i)

func param(i: int): CommandArgument {.inline.} =
  CommandArgument(kind: cakParameter, id: i)
func constVal(val: float64): CommandArgument {.inline.} =
  CommandArgument(kind: cakConst, c: val)

const 
  Zero = CommandArgument(kind: cakZero)
  One = CommandArgument(kind: cakOne)
  MinusOne = CommandArgument(kind: cakMinusOne)
  Nop* = Command(kind: ckNop)

func initIrConverter(paramCount: int): IrConverter =
  result.paramIndex = 0
  result.nextIntermediate = 0
  for i in 0..<paramCount:
    result.derivatives.add Zero

func binaryCommand(c: var IrConverter,
    s: var seq[Command],
    arg1, arg2: CommandArgument,
    kind: CommandKind): CommandArgument =
  result = c.nextInter()
  s.add Command(kind: kind, args: @[arg1, arg2], result: result)

template add(c: var IrConverter,
    s: var seq[Command],
    arg1, arg2: CommandArgument): CommandArgument =
  c.binaryCommand(s, arg1, arg2, ckAdd)
template sub(c: var IrConverter,
    s: var seq[Command],
    arg1, arg2: CommandArgument): CommandArgument =
  c.binaryCommand(s, arg1, arg2, ckSub)
template mul(c: var IrConverter,
    s: var seq[Command],
    arg1, arg2: CommandArgument): CommandArgument =
  c.binaryCommand(s, arg1, arg2, ckMul)
template divCommand(c: var IrConverter,
    s: var seq[Command],
    arg1, arg2: CommandArgument): CommandArgument =
  c.binaryCommand(s, arg1, arg2, ckDiv)
func inv(c: var IrConverter,
    s: var seq[Command],
    arg: CommandArgument): CommandArgument {.inline.} =
  result = c.nextInter()
  s.add Command(kind: ckInv, args: @[arg], result: result)

func callFunc(c: var IrConverter,
    s: var seq[Command],
    source: CommandArgument,
    funcName: string): CommandArgument {.inline.} =
  result = c.nextInter()
  s.add Command(kind: ckFuncCall, args: @[source], result: result, funcName: funcName)
func callFunc(c: var IrConverter,
    s: var seq[Command],
    sources: seq[CommandArgument],
    funcName: string): CommandArgument {.inline.} =
  result = c.nextInter()
  s.add Command(kind: ckFuncCall, args: sources, result: result, funcName: funcName)
func intPower(c: var IrConverter,
    s: var seq[Command],
    source: CommandArgument,
    power: int): CommandArgument {.inline.} =
  result = c.nextInter()
  s.add Command(kind: ckIntPower, args: @[source], result: result, power: power)

func `$`*(a: CommandArgument): string =
  case a.kind:
  of cakVariable: "X" & $a.id
  of cakParameter: "C" & $a.id
  of cakIntermediate: "$" & $a.id
  of cakRegister: "xmm" & $a.id
  of cakMemory: "mem[" & $a.id & "]"
  of cakDerivative: "deriv[" & $a.id & "]"
  of cakResult: "result"
  of cakZero: "0"
  of cakOne: "1"
  of cakMinusOne: "-1"
  of cakConst: $a.c

func `$`*(c: Command): string =
  case c.kind:
  of ckNop: result = "nop"
  of ckAdd, ckSub, ckMul, ckDiv:
    let sign = (case c.kind:
      of ckAdd: " + "
      of ckSub: " - "
      of ckMul: " * "
      of ckDiv: " / "
      else: ""
    )
    result = $c.result & " <- " & $c.args[0] & sign & $c.args[1]
  of ckIntPower:
    result = $c.result & " <- " & $c.args[0] & " ^ " & $c.power
  of ckInv:
    result = $c.result & " <- inv(" & $c.args[0] & ")"
  of ckFuncCall:
    result = $c.result & " <- " & c.funcName & "(" & c.args.join(", ") & ")"
  of ckMov:
    result = $c.result & " <- " & $c.args[0]
  of ckPush:
    result = "push " & c.args.join(", ")
  of ckPop:
    result = "pop " & c.args.join(", ")

func `$`*(p: Program): string =
  result = "forward:\n"
  for c in p.forward:
    result &= "    " & $c & "\n"
  result &= "return " & $p.forwardResult & "\n"
  result &= "backward:\n"
  for c in p.backward:
    result &= "    " & $c & "\n"
  result &= "return " & $p.derivatives & "\n"

func convert(c: var IrConverter, e: Expression,
    currentDeriv: CommandArgument = One): Program =
  template r: untyped = result.forwardResult
  template x: untyped = childProg.forwardResult

  case e.kind:
  of Variable:
    r = variable(e.varIndex)
  of Sum:
    r = c.nextInter()
    if e.constDisabled:
      r = Zero
    else:
      r = param(c.paramIndex)
      c.derivatives[c.paramIndex] = c.add(
        result.backward,
        c.derivatives[c.paramIndex],
        currentDeriv)
      inc c.paramIndex

    for child in e.children:
      let childProg = c.convert(child, currentDeriv)
      result.forward.add childProg.forward
      r = c.add(result.forward, r, x)
      result.backward.add childProg.backward
  of Product:
    r = One
    var childDerivs: seq[CommandArgument]
    var childBackward: seq[Command]
    var childResults: seq[CommandArgument]
    let paramIndex = c.paramIndex
    let thisParam = if e.constDisabled: One else: param(c.paramIndex)
    if not e.constDisabled:
      inc c.paramIndex
    
    for i, child in e.children:
      childDerivs.add c.nextInter()
      let childProg = c.convert(child, childDerivs[i])
      childResults.add x
      result.forward.add childProg.forward
      r = c.mul(result.forward, r, x)
      childBackward.add childProg.backward
    if not e.constDisabled:
      let d = c.mul(result.backward, r, currentDeriv)
      c.derivatives[paramIndex] = c.add(
        result.backward,
        c.derivatives[paramIndex],
        d)
      r = c.mul(result.forward, r, thisParam)
    if e.children.len == 1:
      discard c.mul(result.backward, currentDeriv, thisParam)
      result.backward[^1].result = childDerivs[0]
    else:
      let derC = c.mul(result.backward, currentDeriv, thisParam)
      for i in 0..<e.children.len:
        var deriv = derC
        for j in 0..<e.children.len:
          if i != j:
            deriv = c.mul(result.backward, deriv, childResults[j])
        result.backward[^1].result = childDerivs[i]
    result.backward.add childBackward

  of IntPower:
    let newDeriv = c.nextInter()
    let childProg = c.convert(e.children[0], newDeriv)
    result.forward.add childProg.forward
    if e.children[0].kind == Variable: # case where no derivative is needed
      r = c.intPower(result.forward, x, e.power)
    else:
      let minusOnePower =
        if e.power > 2:
          c.intPower(result.forward, x, e.power-1)
        else:
          x
      r = c.mul(result.forward, minusOnePower, x)
      let powerRule = c.mul(result.backward, constVal(float64(e.power-1)), minusOnePower)
      discard c.mul(result.backward, currentDeriv, powerRule)
      result.backward[^1].result = newDeriv
      result.backward.add childProg.backward
  of Inverse:
    let newDeriv = c.nextInter()
    let childProg = c.convert(e.children[0], newDeriv)
    result.forward.add childProg.forward
    r = c.inv(result.forward, x)
    let minusR = c.mul(result.backward, MinusOne, r)
    let minusSquared = c.mul(result.backward, minusR, r)
    discard c.mul(result.backward, currentDeriv, minusSquared)
    result.backward[^1].result = newDeriv
    result.backward.add childProg.backward
  of Exp:
    let newDeriv = c.nextInter()
    let childProg = c.convert(e.children[0], newDeriv)
    result.forward.add childProg.forward
    r = c.callFunc(result.forward, x, "exp")
    discard c.mul(result.backward, currentDeriv, r)
    result.backward[^1].result = newDeriv
    result.backward.add childProg.backward
  of Ln:
    let newDeriv = c.nextInter()
    let childProg = c.convert(e.children[0], newDeriv)
    result.forward.add childProg.forward
    r = c.callFunc(result.forward, x, "log")
    discard c.divCommand(result.backward, currentDeriv, x)
    result.backward[^1].result = newDeriv
    result.backward.add childProg.backward
  of Sqrt:
    let newDeriv = c.nextInter()
    let childProg = c.convert(e.children[0], newDeriv)
    result.forward.add childProg.forward
    r = c.callFunc(result.forward, x, "sqrt")
    let half = c.divCommand(result.backward, constVal(0.5), r)
    discard c.mul(result.backward, currentDeriv, half)
    result.backward[^1].result = newDeriv
    result.backward.add childProg.backward
  of Sin:
    let newDeriv = c.nextInter()
    let childProg = c.convert(e.children[0], newDeriv)
    result.forward.add childProg.forward
    r = c.callFunc(result.forward, x, "sin")
    let cos = c.callFunc(result.backward, x, "cos")
    discard c.mul(result.backward, currentDeriv, cos)
    result.backward[^1].result = newDeriv
    result.backward.add childProg.backward
  of Cos:
    let newDeriv = c.nextInter()
    let childProg = c.convert(e.children[0], newDeriv)
    result.forward.add childProg.forward
    r = c.callFunc(result.forward, x, "cos")
    let sin = c.callFunc(result.backward, x, "sin")
    let minusSin = c.mul(result.backward, MinusOne, sin)
    discard c.mul(result.backward, currentDeriv, minusSin)
    result.backward[^1].result = newDeriv
    result.backward.add childProg.backward
  of Asin:
    let newDeriv = c.nextInter()
    let childProg = c.convert(e.children[0], newDeriv)
    result.forward.add childProg.forward
    r = c.callFunc(result.forward, x, "asin")
    let squared = c.mul(result.backward, x, x)
    let oneMinusSquared = c.sub(result.backward, One, squared)
    let sqrt = c.callFunc(result.backward, oneMinusSquared, "sqrt")
    discard c.divCommand(result.backward, currentDeriv, sqrt)
    result.backward[^1].result = newDeriv
    result.backward.add childProg.backward
  of Acos:
    let newDeriv = c.nextInter()
    let childProg = c.convert(e.children[0], newDeriv)
    result.forward.add childProg.forward
    r = c.callFunc(result.forward, x, "acos")
    let squared = c.mul(result.backward, x, x)
    let oneMinusSquared = c.sub(result.backward, One, squared)
    let sqrt = c.callFunc(result.backward, oneMinusSquared, "sqrt")
    let minusInv = c.divCommand(result.backward, MinusOne, sqrt)
    discard c.mul(result.backward, currentDeriv, minusInv)
    result.backward[^1].result = newDeriv
    result.backward.add childProg.backward
  of Erf:
    let newDeriv = c.nextInter()
    let childProg = c.convert(e.children[0], newDeriv)
    result.forward.add childProg.forward
    r = c.callFunc(result.forward, x, "erf")
    let squared = c.mul(result.backward, x, x)
    let minusSquared = c.mul(result.backward, MinusOne, squared)
    let exp = c.callFunc(result.backward, minusSquared, "exp")
    let expCoef = c.mul(result.backward, constVal(2/sqrt(PI)), exp)
    discard c.mul(result.backward, currentDeriv, expCoef)
    result.backward[^1].result = newDeriv
    result.backward.add childProg.backward
  of Arctan2:
    let newDeriv1 = c.nextInter()
    let newDeriv2 = c.nextInter()
    let childProg1 = c.convert(e.children[0], newDeriv1)
    result.forward.add childProg1.forward
    let childProg2 = c.convert(e.children[1], newDeriv2)
    result.forward.add childProg2.forward
    template x: untyped = childProg2.forwardResult
    template y: untyped = childProg1.forwardResult
    r = c.callFunc(result.forward, @[y, x], "atan2")
    let squaredX = c.mul(result.backward, x, x)
    let squaredY = c.mul(result.backward, y, y)
    let denom = c.add(result.backward, squaredX, squaredY)
    let minusY = c.mul(result.backward, MinusOne, y)
    discard c.divCommand(result.backward, minusY, denom)
    result.backward[^1].result = newDeriv2
    discard c.divCommand(result.backward, x, denom)
    result.backward[^1].result = newDeriv1
    result.backward.add childProg1.backward
    result.backward.add childProg2.backward
  of Gaussian:
    let paramIndex = c.paramIndex
    let mean = param(paramIndex)
    let std = param(paramIndex+1)
    c.paramIndex += 2
    let newDeriv = c.nextInter()
    let childProg = c.convert(e.children[0], newDeriv)
    result.forward.add childProg.forward

    let diff = c.sub(result.forward, x, mean)
    let frac = c.divCommand(result.forward, diff, std)
    let fracSquared = c.mul(result.forward, frac, frac)
    let minusFrac = c.mul(result.forward, MinusOne, fracSquared)
    r = c.callFunc(result.forward, minusFrac, "exp")
    let stdSquared = c.mul(result.backward, std, std)
    let fracM = c.divCommand(result.backward, diff, stdSquared)
    let twiceG = c.add(result.backward, r, r)
    let deriv = c.mul(result.backward, currentDeriv, twiceG)
    let dMean = c.mul(result.backward, deriv, fracM)
    c.derivatives[paramIndex] = c.add(
        result.backward,
        c.derivatives[paramIndex],
        dMean)
    discard c.mul(result.backward, MinusOne, dMean)
    result.backward[^1].result = newDeriv
    let dStd = c.mul(result.backward, dMean, fracM)
    c.derivatives[paramIndex+1] = c.add(
        result.backward,
        c.derivatives[paramIndex+1],
        dStd)
    result.backward.add childProg.backward

func convertToIr*(e: Expression): Program =
  var ir = initIrConverter(e.paramCount)
  result = ir.convert(e)
  result.derivatives = ir.derivatives


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
  echo e

  #var ir = initIrConverter(e.paramCount)
  let p = e.convertToIr()
  echo p
  #echo ir.derivatives
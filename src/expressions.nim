import math
from strutils import join
import strformat
import tables, deques
import smallset
import matrix, variabledata
import sugar, options

export smallset, Number

type
  ExprKind* {.pure.} = enum
    # Special expressions
    Variable
    Sum
    Product
    IntPower
    # One variable functions
    Inverse
    Exp
    Ln
    Sqrt
    Sin
    Cos
    Asin
    Acos
    Erf
    # Two variable functions
    Arctan2
    # Special functions
    Gaussian
  Expression* {.acyclic.} = ref object
    case kind*: ExprKind:
      of Variable:
        varIndex*: int
      of Sum, Product:
        constDisabled*: bool
      of IntPower:
        power*: int
      else:
        discard
    children*: SmallSet[Expression]
    lastValue*: Number
  SerializedExpr* = seq[int]

const 
  TotalKinds* = high(ExprKind).int - low(ExprKind).int + 2
  UnaryKinds* = {Inverse, Exp, Ln, Sqrt, Sin, Cos, Asin, Acos, Erf, Gaussian}
  BinaryKinds* = {Arctan2}

func structureCmp*(a, b: Expression): int =
  if a.kind != b.kind:
    return cmp(a.kind, b.kind)
  if a.children.len != b.children.len:
    return cmp(a.children.len, b.children.len)

  case a.kind:
  of Variable: return cmp(a.varIndex, b.varIndex)
  of Sum, Product, Arctan2:
    for i in 0..<a.children.len:
      let c = structureCmp(a.children[i], b.children[i])
      if c != 0: return c
    return 0
  of IntPower:
    if a.power != b.power: return cmp(a.power, b.power)
    return structureCmp(a.children[0], b.children[0])
  else:
    return structureCmp(a.children[0], b.children[0])

func equivalenceCmp*(a, b: Expression): int =
  if a.kind != b.kind:
    return cmp(a.kind, b.kind)
  if a.children.len != b.children.len:
    return cmp(a.children.len, b.children.len)

  case a.kind:
  of Variable: return cmp(a.varIndex, b.varIndex)
  of Sum, Product, Arctan2:
    for i in 0..<a.children.len:
      let c2 = equivalenceCmp(a.children[i], b.children[i])
      if c2 != 0: return c2
    return 0
  of IntPower:
    if a.power != b.power: return cmp(a.power, b.power)
    return equivalenceCmp(a.children[0], b.children[0])
  else:
    let c = equivalenceCmp(a.children[0], b.children[0])
    if c != 0: return c
    else: return cmp(a, b)

func initVariable*(index: int): Expression {.inline.} = Expression(
  kind: Variable,
  varIndex: index,
  children: initSmallSetWithNoCmp[Expression]()
)
func initBigExpr*(kind: ExprKind, constDisabled=false): Expression {.inline.} =
  assert(kind in {Sum, Product}, "Only sum and product are big")
  {.cast(uncheckedAssign).}:
    Expression(
      kind: kind,
      constDisabled: constDisabled,
      children: initSmallSet[Expression](equivalenceCmp)
    )
func initUnaryExpr*(kind: ExprKind): Expression {.inline.} =
  assert(kind in UnaryKinds, "This is not an unary expression")
  Expression(
    kind: kind,
    children: initSmallSetWithNoCmp[Expression]()
  )
func initBinaryExpr*(kind: ExprKind): Expression {.inline.} =
  assert(kind in BinaryKinds, "This is not a binary expression")
  Expression(
    kind: kind,
    children: initSmallSetWithNoCmp[Expression]()
  )
func initIntPower*(power: int): Expression {.inline.} =
  assert(power < TotalKinds, "Power exceeds TotalKinds")
  Expression(
    kind: IntPower,
    power: power,
    children: initSmallSetWithNoCmp[Expression]()
  )
func withChildren*(e: Expression, children: varargs[Expression]): Expression =
  for c in children:
    e.children.add c
  result = e
func nested*(exprs: varargs[Expression]): Expression =
  for i in 0..<exprs.len - 1:
    exprs[i].children.add exprs[i+1]
  result = exprs[0]

func `$`*(e: Expression): string

func eval*(e: Expression,
    vars: VariableSlice,
    params: Vector,
    paramIndex: var int): Number =
  # debugEcho e
  case e.kind:
  of Variable:
    e.lastValue = vars[e.varIndex]
  of Sum:
    if e.constDisabled:
      e.lastValue = 0
    else:
      e.lastValue = params[paramIndex]
      inc paramIndex
    for child in e.children:
      e.lastValue += child.eval(vars, params, paramIndex)
  of Product:
    if e.constDisabled:
      e.lastValue = 1
    else:
      e.lastValue = params[paramIndex]
      inc paramIndex
    for child in e.children:
      e.lastValue *= child.eval(vars, params, paramIndex)
  of Gaussian:
    let mean = params[paramIndex]
    let std = params[paramIndex+1]
    paramIndex += 2
    let x = e.children[0].eval(vars, params, paramIndex)
    e.lastValue = exp(-pow((x - mean) / std, 2))
  of Arctan2:
    let y = e.children[0].eval(vars, params, paramIndex)
    let x = e.children[1].eval(vars, params, paramIndex)
    e.lastValue = arctan2(y, x)
  else:
    let x = e.children[0].eval(vars, params, paramIndex)
    case e.kind:
    of IntPower:
      e.lastValue = x.pow(e.power.Number)
    of Inverse:
      e.lastValue = 1/x
    of Exp:
      e.lastValue = exp(x)
    of Ln:
      e.lastValue = ln(abs(x))
    of Sqrt:
      e.lastValue = sqrt(abs(x))
    of Sin:
      e.lastValue = sin(x)
    of Cos:
      e.lastValue = cos(x)
    of Asin:
      e.lastValue = sin(x)
    of Acos:
      e.lastValue = cos(x)
    of Erf:
      e.lastValue = erf(x)
    else: discard
  result = e.lastValue

func evalDerivs*(e: Expression,
    vars: VariableSlice,
    params: Vector,
    paramIndex: var int,
    result: var Vector,
    currentDeriv: Number = 1) =
  template evalDerivs(e: Expression, deriv: Number): untyped =
      e.evalDerivs(vars, params, paramIndex, result, currentDeriv * deriv)
  template x: untyped = e.children[0].lastValue

  case e.kind:
  of Variable: discard
  of Sum:
    if not e.constDisabled:
      result[paramIndex] += currentDeriv
      inc paramIndex
    for child in e.children:
      child.evalDerivs(1)
  of Product:
    let thisParam = if not e.constDisabled: params[paramIndex] else: 1
    if not e.constDisabled:
      if abs(thisParam) > 1e-10:
        result[paramIndex] += currentDeriv * e.lastValue / thisParam
      else:
        var d = currentDeriv
        for j, otherChild in e.children:
          d *= otherChild.lastValue
        result[paramIndex] += d
      inc paramIndex
    for i, child in e.children:
      let newDeriv = (
        if abs(child.lastValue) < 1e-5:
          var d = thisParam
          for j, otherChild in e.children:
            if i != j:
              d *= otherChild.lastValue
          d
        else:
          e.lastValue / child.lastValue
        )
      child.evalDerivs(newDeriv)
    
  of IntPower:
    if abs(x) < 1e-10:
      e.children[0].evalDerivs(0)
    else:
      e.children[0].evalDerivs(e.power.Number * e.lastValue / x)
  of Inverse:
    e.children[0].evalDerivs(-e.lastValue.pow(2))
  of Exp:
    e.children[0].evalDerivs(e.lastValue)
  of Ln:
    e.children[0].evalDerivs(1 / x)
  of Sqrt:
    e.children[0].evalDerivs(0.5 * x.pow(-1/2))
  of Sin:
    e.children[0].evalDerivs(cos(x))
  of Cos:
    e.children[0].evalDerivs(-sin(x))
  of Asin:
    e.children[0].evalDerivs(1/sqrt(1 - x.pow(2)))
  of Acos:
    e.children[0].evalDerivs(-1/sqrt(1 - x.pow(2)))
  of Erf:
    const coef = 2/sqrt(PI)
    e.children[0].evalDerivs(coef*exp(-x.pow(2)))
  of Gaussian:
    let
      mean = params[paramIndex]
      std = params[paramIndex+1]
      this = e.lastValue
      dmean = this * 2*(x-mean) / std.pow(2)
      dstd = this * 2*pow(x-mean, 2)/std.pow(3)
      dx = -dmean
    result[paramIndex] += currentDeriv * dmean
    result[paramIndex+1] += currentDeriv * dstd
    paramIndex += 2
    e.children[0].evalDerivs(dx)
  of Arctan2:
    template x: untyped = e.children[1].lastValue
    template y: untyped = e.children[0].lastValue
    let r = x*x + y*y
    e.children[0].evalDerivs(x/r)
    e.children[1].evalDerivs(-y/r)


func toString*(e: Expression, paramIndex: var int): string =
  template childStr: untyped = e.children[0].toString(paramIndex)

  case e.kind:
  of Variable: result = fmt"x{e.varIndex}"
  of Sum, Product:
    if e.constDisabled: result = ""
    else:
      result = fmt"c{paramIndex}" & (if e.kind == Sum: " + " else: " * ")
      inc paramIndex
    
    let children = collect(newSeqOfCap(e.children.len)):
      for child in e.children:
        child.toString(paramIndex)
    result &= children.join(if e.kind == Sum: " + " else: " * ")
  of IntPower:
    result = childStr & " ^ " & $e.power
  of Inverse:
    result = fmt"1/({childStr})"
  of Gaussian:
    let p = paramIndex
    paramIndex += 2
    result = fmt"G({childStr};c{p};c{p+1})"
  of Arctan2:
    template childStr2: untyped = e.children[1].toString(paramIndex)
    result = fmt"atan2({childStr};{childStr2})"
  of Exp, Ln, Sqrt, Sin, Cos, Asin, Acos, Erf:
    const nameTable = {
      Exp: "exp", Ln: "ln", Sqrt: "sqrt",
      Sin: "sin", Cos: "cos",
      Asin: "asin", Acos: "acos", Erf: "erf"}.toTable
    result = fmt"{nameTable[e.kind]}({childStr})"

func toStructureString*(e: Expression): string =
  template childStr: untyped = e.children[0].toStructureString()

  case e.kind:
  of Variable: result = fmt"x{e.varIndex}"
  of Sum, Product:
    let name = if e.constDisabled: $e.kind else: $e.kind & "C"
    result = fmt"{name}("
    
    let children = collect(newSeqOfCap(e.children.len)):
      for child in e.children:
        child.toStructureString()
    result &= children.join(", ") & ")"
  of IntPower:
    result = childStr & " ^ " & $e.power
  of Inverse:
    result = fmt"1/({childStr})"
  of Gaussian:
    result = fmt"G({childStr})"
  of Arctan2:
    template childStr2: untyped = e.children[1].toStructureString()
    result = fmt"atan2({childStr};{childStr2})"
  of Exp, Ln, Sqrt, Sin, Cos, Asin, Acos, Erf:
    const nameTable = {
      Exp: "exp", Ln: "ln", Sqrt: "sqrt",
      Sin: "sin", Cos: "cos",
      Asin: "asin", Acos: "acos", Erf: "erf"}.toTable
    result = fmt"{nameTable[e.kind]}({childStr})"

func `$`*(e: Expression): string =
  var paramIndex = 0
  e.toString(paramIndex)

func toString*(e: Expression, params: Vector, paramIndex: var int): string =
  template childStr: untyped = e.children[0].toString(params, paramIndex)

  case e.kind:
  of ExprKind.Variable: result = fmt"x{e.varIndex}"
  of Sum, Product:
    if e.constDisabled: result = ""
    else:
      result = fmt"{params[paramIndex]:.10f}" & (if e.kind == Sum: " + " else: " * ")
      inc paramIndex
    
    let children = collect(newSeqOfCap(e.children.len)):
      for child in e.children:
        child.toString(params, paramIndex)
    result &= children.join(if e.kind == Sum: " + " else: " * ")
  of IntPower:
    result = childStr & " ^ " & $e.power
  of Inverse:
    result = fmt"1/({childStr})"
  of Gaussian:
    let p = paramIndex
    paramIndex += 2
    result = fmt"G({childStr};{params[p]:.4f};{abs(params[p+1]):.4f})"
  of Arctan2:
    template childStr2: untyped = e.children[1].toString(params, paramIndex)
    result = fmt"atan2({childStr};{childStr2})"
  of Exp, Ln, Sqrt, Sin, Cos, Asin, Acos, Erf:
    const nameTable = {
      Exp: "exp", Ln: "ln", Sqrt: "sqrt",
      Sin: "sin", Cos: "cos",
      Asin: "asin", Acos: "acos", Erf: "erf"}.toTable
    result = fmt"{nameTable[e.kind]}({childStr})"

template copyMap*(input: Expression, f: untyped, result: untyped): untyped =
  let children = (
    if input.kind in {Sum, Product}:
      initSmallSet[Expression](equivalenceCmp)
    else:
      initSmallSetWithNoCmp[Expression]()
    )
  result = Expression(kind: e.kind, children: children)
#  result.kind = e.kind
  case result.kind:
    of ExprKind.Variable: result.varIndex = e.varIndex
    of Sum, Product: result.constDisabled = e.constDisabled
    of IntPower: result.power = e.power
    else: discard
  
  for child in e.children:
    result.children.add f(child)

func copy*(e: Expression): Expression {.inline.} =
  e.copyMap(copy, result)
  # debugEcho e, " -> ", result

func paramCount*(e: Expression): int =
  case e.kind:
  of Variable: result = 0
  of Sum, Product:
    result = if e.constDisabled: 0 else: 1
  of Gaussian:
    result = 2
  else: discard

  for child in e.children:
    result += child.paramCount

func serialize(e: Expression, result: var SerializedExpr) =
  if e.kind == Variable:
    result.add (TotalKinds + e.varIndex)
  else:
    result.add(e.kind.int + 1)
    if e.kind == IntPower:
      result.add e.power
    elif e.kind in {Sum, Product}:
      if e.constDisabled:
        result.add 1
      else:
        result.add 2
    for child in e.children:
      child.serialize(result)
    if e.kind in {Sum, Product}:
      result.add 1

func deserializeExpr*(s: SerializedExpr, i: var int): Expression =
  if s[i] >= TotalKinds:
    result = initVariable(s[i] - TotalKinds)
    inc i
  else:
    let kind = ExprKind(s[i] - 1)
    inc i
    if kind == IntPower:
      result = initIntPower(s[i])
      inc i
      result.children.add s.deserializeExpr(i)
    elif kind in {Sum, Product}:
      let constDisabled = s[i] == 1
      inc i
      result = initBigExpr(kind, constDisabled)
      while i < s.len and s[i] > 1:
        result.children.add s.deserializeExpr(i)
      if i < s.len and s[i] == 1:
        inc i
    elif kind in BinaryKinds:
      result = initBinaryExpr(kind)
      result.children.add s.deserializeExpr(i)
      result.children.add s.deserializeExpr(i)
    else:
      result = initUnaryExpr(kind)
      result.children.add s.deserializeExpr(i)

  

func serialized*(e: Expression): SerializedExpr =
  e.serialize(result)
  for i in countdown(result.len - 1, 0):
    if result[i] == 1: continue
    result.setLen(i+1)
    break

func complexity*(e: SerializedExpr): int =
  var i = 0
  while i < e.len:
    if e[i] >= TotalKinds:
      result += 1
    elif e[i] in {0, 1}:
      discard
    else:
      let kind = ExprKind(e[i]-1)
      if kind == IntPower:
        i.inc
        result += e[i]
      elif kind in {Sum, Product}:
        i.inc
      elif kind == Gaussian:
        result += 5
      elif kind in UnaryKinds or kind in BinaryKinds:
        result += 3
    i.inc

func copyAndReplace*(e, pattern, replacement: Expression): Expression {.inline.} =
  func f(e: Expression): Expression =
    if e == pattern:
      return replacement
    e.copyMap(f, result)
  f(e)

func copyAndDelete*(e, pattern: Expression): Expression {.inline.} =
  func f(e: Expression): Option[Expression] =
    if e == pattern:
      return none(Expression)
    var someExpr: Expression
    e.copyMap(f, someExpr)
    result = some(someExpr)
  f(e).get()

# func copyAndReplace*(e, pattern, replacement: Expression): Expression =
#   if e == pattern:
#     return replacement
  
#   result = Expression(
#     kind: e.kind,
#     children: initSmallSet[Expression](equivalenceCmp)
#   )
#   case e.kind:
#     of Variable: result.varIndex = e.varIndex
#     of Sum, Product: result.constDisabled = e.constDisabled
#     of IntPower: result.power = e.power
#     else: discard
#   for child in e.children:
#     result.children.add child.copyAndReplace(pattern, replacement)

iterator nodes*(e: Expression): Expression =
  var stack = newSeq[(Expression, int)]()
  var n = e
  while stack.len > 0:
    yield n
    if n.children.len > 0:
      stack.add (n, 0)
      n = n.children[0]
    else:
      while stack.len > 0:
        let (p, i) = stack.pop()
        if i+1 < p.children.len:
          n = p.children[i+1]
          stack.add (p, i+1)
          break


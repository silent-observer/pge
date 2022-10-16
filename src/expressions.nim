import math
from strutils import join
import strformat
import tables, deques
import smallset
import matrix, variabledata
import sugar

export smallset, Number

type
  ExprKind* {.pure.} = enum
    Variable
    Sum
    Product
    IntPower
    Inverse
    Exp
    Ln
    Sqrt
    Sin
    Cos
    Asin
    Acos
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
  TotalKinds* = high(ExprKind).int - low(ExprKind).int + 1
  UnaryKinds* = {Inverse, Exp, Ln, Sqrt, Sin, Cos, Asin, Acos}

func structureCmp*(a, b: Expression): int =
  if a.kind != b.kind:
    return cmp(a.kind, b.kind)
  if a.children.len != b.children.len:
    return cmp(a.children.len, b.children.len)

  case a.kind:
  of Variable: return cmp(a.varIndex, b.varIndex)
  of Sum, Product:
    for i in 0..<a.children.len:
      let c = cmp(a.children[i], b.children[i])
      if c != 0: return c
  of IntPower:
    if a.power != b.power: return cmp(a.power, b.power)
    return cmp(a.children[0], b.children[0])
  else:
    return cmp(a.children[0], b.children[0])

func equivalenceCmp*(a, b: Expression): int =
  if a.kind == Variable and b.kind == Variable:
    cmp(a.varIndex, b.varIndex)
  elif a.kind == Variable or b.kind == Variable:
    cmp(a.kind, b.kind)
  else:
    let c = structureCmp(a, b)
    if c == 0:
      cmp(unsafeAddr a[], unsafeAddr b[])
    else:
      c

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
  assert(kind in Inverse..Acos, "This is not an unary expression")
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
  of Exp, Ln, Sqrt, Sin, Cos, Asin, Acos:
    const nameTable = {
      Exp: "exp", Ln: "ln", Sqrt: "sqrt",
      Sin: "sin", Cos: "cos",
      Asin: "asin", Acos: "acos"}.toTable
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
      result = fmt"{params[paramIndex]:.4f}" & (if e.kind == Sum: " + " else: " * ")
      inc paramIndex
    
    let children = collect(newSeqOfCap(e.children.len)):
      for child in e.children:
        child.toString(params, paramIndex)
    result &= children.join(if e.kind == Sum: " + " else: " * ")
  of IntPower:
    result = childStr & " ^ " & $e.power
  of Inverse:
    result = fmt"1/({childStr})"
  of Exp, Ln, Sqrt, Sin, Cos, Asin, Acos:
    const nameTable = {
      Exp: "exp", Ln: "ln", Sqrt: "sqrt",
      Sin: "sin", Cos: "cos",
      Asin: "asin", Acos: "acos"}.toTable
    result = fmt"{nameTable[e.kind]}({childStr})"

template copyMap*(input: Expression, f: untyped): untyped =
  result = Expression(kind: e.kind)
#  result.kind = e.kind
  case result.kind:
    of ExprKind.Variable: result.varIndex = e.varIndex
    of Sum, Product: result.constDisabled = e.constDisabled
    of IntPower: result.power = e.power
    else: discard
  
  if result.kind in {Sum, Product}:
    result.children = initSmallSet[Expression](equivalenceCmp)
  else:
    result.children = initSmallSetWithNoCmp[Expression]()
  
  for child in e.children:
    result.children.add f(child)

func copy*(e: Expression): Expression {.inline.} =
  e.copyMap(copy)
  # debugEcho e, " -> ", result

func paramCount*(e: Expression): int =
  case e.kind:
  of Variable: result = 0
  of Sum, Product:
    result = if e.constDisabled: 0 else: 1
    for child in e.children:
      result += child.paramCount
  else:
    result = e.children[0].paramCount

func serialize(e: Expression, result: var SerializedExpr) =
  if e.kind == Variable:
    result.add (TotalKinds + e.varIndex)
  else:
    result.add e.kind.int
    if e.kind == IntPower:
      result.add e.power
    for child in e.children:
      child.serialize(result)
    if e.kind in {Sum, Product}:
      result.add 0

func serialized*(e: Expression): SerializedExpr =
  e.serialize(result)
  for i in countdown(result.len - 1, 0):
    if result[i] == 0: continue
    result.setLen(i+1)
    break

func complexity*(e: SerializedExpr): int =
  var i = 0
  while i < e.len:
    if e[i] > TotalKinds:
      result += 2
    elif e[i] == IntPower.int:
      i.inc
      result += e[i]
    elif e[i] in int(Inverse)..int(Acos):
      result += 5
    i.inc

func copyAndReplace*(e, pattern, replacement: Expression): Expression {.inline.} =
  func f(e: Expression): Expression =
    if e == pattern:
      return replacement
    e.copyMap(f)
  f(e)

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


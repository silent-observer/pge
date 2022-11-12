import expressions
import matrix
import strformat

type TermData* = object
  e*: Expression
  nonlinearParams*: int

type LinearFormula* = object
  terms*: SmallSet[TermData]
  id*: int

func `$`*(f: LinearFormula): string {.inline.} = 
  var paramIndex = 0
  for i, term in f.terms:
    result &= fmt"a{i}" & " * " & 
      term.e.toString(paramIndex) & " + "
  result &= fmt"a{f.terms.len}"

func termDataCmp*(a, b: TermData): int = 
  result = equivalenceCmp(a.e, b.e)
  # if result < 0:
  #   debugEcho "    ", a.e.toStructureString, " < ", b.e.toStructureString
  # elif result == 0:
  #   debugEcho "    ", a.e.toStructureString, " = ", b.e.toStructureString
  # else:
  #   debugEcho "    ", a.e.toStructureString, " > ", b.e.toStructureString

func initLinearFormula*(terms: varargs[Expression]): LinearFormula =
  result.terms = initSmallSet[TermData](termDataCmp)
  for term in terms:
    assert term.kind == Product, "Linearized expression must be a sum of products"
    let newTerm = term.copy()
    newTerm.constDisabled = true
    result.terms.add TermData(
      e: newTerm,
      nonlinearParams: newTerm.paramCount
    )
  result.id = 1


# func linearize*(e: Expression): LinearFormula =
#   assert e.kind == Sum, "Linearized expression must be a sum of products"
#   for term in e.children:
#     assert term.kind == Product, "Linearized expression must be a sum of products"
#     let newTerm = term.copy()
#     newTerm.constDisabled = true
#     result.terms.add newTerm
#     result.nonlinearParams.add newTerm.paramCount

func copy*(f: LinearFormula): LinearFormula {.inline.} =
  result.terms = initSmallSet[TermData](termDataCmp)
  for term in f.terms:
    result.terms.add TermData(
      e: term.e.copy(),
      nonlinearParams: term.nonlinearParams
    )
  result.id = f.id

func serialized*(f: LinearFormula): SerializedExpr =
  for i, term in f.terms:
    if i > 0: result.add 0
    result.add term.e.serialized()

func deserializeFormula*(s: SerializedExpr): LinearFormula =
  result.terms = initSmallSet[TermData](termDataCmp)
  var i = 0
  while i < s.len:
    if s[i] == 0:
      inc i
    let newTerm = s.deserializeExpr(i)
    result.terms.add TermData(
      e: newTerm,
      nonlinearParams: newTerm.paramCount
    )

# func evalOnly*(f: LinearFormula, vars, linearParams, nonlinearParams: seq[Number]): Number =
#   result = linearParams[^1]
#   var paramIndex = 0
#   for i, term in f.terms:
#     let v = term.eval(vars, nonlinearParams, paramIndex)
#     result += linearParams[i] * v

# func evalWithDerivs*(f: LinearFormula, vars, linearParams, 
#     nonlinearParams: seq[Number], derivs: var seq[Number]): Number =
#   result = linearParams[^1]
#   var paramIndex = 0
#   for i, term in f.terms:
#     let startParamIndex = paramIndex
#     let v = term.eval(vars, nonlinearParams, paramIndex)
#     paramIndex = startParamIndex
#     term.evalDerivs(vars, nonlinearParams, paramIndex, derivs[i, _])
#     result += linearParams[i] * v

func toString*(f: LinearFormula, linearParams, nonlinearParams: Vector): string =
  var paramIndex = 0
  for i, term in f.terms:
    result &= fmt"{linearParams[i]:.4f}" & " * " & 
      term.e.toString(nonlinearParams, paramIndex) & " + "
  result &= fmt"{linearParams[f.terms.len]:.4f}"

when isMainModule:
  let e1 = initBigExpr(Product).withChildren(
    initVariable(0)
  )
  let e2 = initBigExpr(Product).withChildren(
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
  let e3 = nested(
    initBigExpr(Product),
    initIntPower(3),
    initVariable(0)
  )
  let f = initLinearFormula(e1, e2, e3)
  echo f
  let s = f.serialized()
  echo s
  let f2 = s.deserializeFormula()
  echo f2

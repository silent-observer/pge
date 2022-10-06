import expressions
import matrix
import strformat

type LinearFormula* = object
  terms*: seq[Expression]
  nonlinearParams*: seq[int]

func `$`*(f: LinearFormula): string {.inline.} = fmt"LF({f.terms})"

func linearize*(e: Expression): LinearFormula =
  assert e.kind == Sum, "Linearized expression must be a sum of products"
  for term in e.children:
    assert term.kind == Product, "Linearized expression must be a sum of products"
    let newTerm = term.copy()
    newTerm.constDisabled = true
    result.terms.add newTerm
    result.nonlinearParams.add newTerm.paramCount

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
      term.toString(nonlinearParams, paramIndex) & " + "
  result &= fmt"{linearParams[f.terms.len]:.4f}"
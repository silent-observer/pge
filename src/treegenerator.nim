import expressions
import formula
from sequtils import repeat

type
  BasisFunction* {.pure.} = enum
    IntPower
    Inverse
    Exp
    Ln
    Sqrt
    Sin
    Cos
    Asin
    Acos
  Basis* = set[BasisFunction]

type BasisEntry = object
  kind: ExprKind
  addConstDisabled, mulConstDisabled: bool

const BasisData: array[BasisFunction, BasisEntry] = [
  BasisEntry(kind: ExprKind.IntPower, addConstDisabled: false, mulConstDisabled: false),
  BasisEntry(kind: ExprKind.Inverse, addConstDisabled: false, mulConstDisabled: true),
  BasisEntry(kind: ExprKind.Exp, addConstDisabled: true, mulConstDisabled: false),
  BasisEntry(kind: ExprKind.Ln, addConstDisabled: false, mulConstDisabled: false),
  BasisEntry(kind: ExprKind.Sqrt, addConstDisabled: false, mulConstDisabled: true),
  BasisEntry(kind: ExprKind.Sin, addConstDisabled: false, mulConstDisabled: false),
  BasisEntry(kind: ExprKind.Cos, addConstDisabled: false, mulConstDisabled: false),
  BasisEntry(kind: ExprKind.Asin, addConstDisabled: false, mulConstDisabled: false),
  BasisEntry(kind: ExprKind.Acos, addConstDisabled: false, mulConstDisabled: false),
]

func variableProductionRules(allowed: Basis, varCount: int): seq[Expression] =
  for f in allowed - {BasisFunction.IntPower}:
    let data = BasisData[f]
    for v in 0..<varCount:
      result.add nested(
        initUnaryExpr(data.kind),
        initBigExpr(Sum, data.addConstDisabled),
        initBigExpr(Product, data.mulConstDisabled),
        initVariable(v)
      )
  if BasisFunction.IntPower in allowed:
    for v in 0..<varCount:
      result.add initIntPower(2).withChildren(initVariable(v))

func applyProductionRules(e: Expression, allowed: Basis, varCount: int): seq[Expression] =
  if e.kind == Variable:
    result = variableProductionRules(allowed, varCount)
  elif e.kind in {Sum, Product}:
    var vars = true.repeat(varCount)
    for child in e.children:
      if child.kind == Variable:
        vars[child.varIndex] = false
      elif child.kind == Product and
          child.children.len == 1 and
          child.children[0].kind == Variable:
        vars[child.children[0].varIndex] = false
    for v in 0..<varCount:
      if vars[v]:
        let copy = e.copy()
        if e.kind == Sum:
          result.add copy.nested(
            initBigExpr(Product, false),
            initVariable(v)
          )
        else:
          result.add copy.withChildren(initVariable(v))
  elif e.kind == ExprKind.IntPower:
    let copy = e.copy()
    inc copy.power
    result.add copy

func generateTrees(e, node: Expression, basis, allowed: Basis, varCount: int): seq[Expression] =
  if node.kind in UnaryKinds + {ExprKind.IntPower}:
    var newAllowed = allowed
    if node.kind == ExprKind.IntPower:
      newAllowed.excl BasisFunction.Exp
      newAllowed.excl BasisFunction.Sqrt
      newAllowed.excl BasisFunction.Inverse
      newAllowed.excl BasisFunction.IntPower
    else:
      newAllowed.incl BasisFunction.Exp
      newAllowed.incl BasisFunction.Sqrt
      newAllowed.incl BasisFunction.IntPower
      if node.kind in {ExprKind.Inverse, ExprKind.Sqrt}:
        newAllowed.excl BasisFunction.Inverse
      else:
        newAllowed.incl BasisFunction.Inverse
    newAllowed = newAllowed * basis
    result = e.generateTrees(node.children[0], basis, newAllowed, varCount)
  else:
    if node.kind in {Sum, Product}:
      var newAllowed = allowed
      if node.kind == Product:
        for child in node.children:
          case child.kind
          of ExprKind.Inverse:
            newAllowed.excl BasisFunction.Inverse
          of ExprKind.Exp:
            newAllowed.excl BasisFunction.Exp
          of ExprKind.Sqrt:
            newAllowed.excl BasisFunction.Sqrt
          else: discard
      for child in node.children:
        result.add e.generateTrees(child, basis, newAllowed, varCount)
    
    let subTrees = node.applyProductionRules(allowed, varCount)
    for subTree in subTrees:
      result.add e.copyAndReplace(node, subTree)

func generateTrees*(e: Expression, basis: Basis, varCount: int): seq[Expression] {.inline.} =
  generateTrees(e, e, basis, basis, varCount)

func generateFormulas*(f: LinearFormula, basis: Basis, varCount: int): seq[LinearFormula] =
  var vars = true.repeat(varCount)
  for i, term in f.terms:
    if term.e.kind == Product and
        term.e.children.len == 1 and
        term.e.children[0].kind == Variable:
      vars[term.e.children[0].varIndex] = false
    for modifiedTerm in term.e.generateTrees(basis, varCount):
      var newFormula = initLinearFormula()
      for j, term2 in f.terms:
        if i != j: 
          newFormula.terms.add TermData(
            e: term2.e.copy(),
            nonlinearParams: term2.nonlinearParams
          )
        else:
          newFormula.terms.add TermData(
            e: modifiedTerm,
            nonlinearParams: modifiedTerm.paramCount
          )
      
      result.add newFormula
  
  for v in 0..<varCount:
    if vars[v]:
      var copy = f.copy()
      copy.terms.add TermData(
        e: initBigExpr(Product, constDisabled=true)
            .withChildren(initVariable(v)),
        nonlinearParams: 0
      )
      result.add copy


import expressions, formula

func fixIntPowers(e: Expression): Expression =
  if e.kind != Product: return e
  result = e

  var toRemove: seq[int] = @[]
  for i, child1 in result.children:
    if child1.kind == IntPower and child1.children[0].kind == Variable:
      let variable = child1.children[0].varIndex
      for j, child2 in result.children:
        if child2.kind == Variable and child2.varIndex == variable:
          inc child1.power
          toRemove.add j
  let childCopy = result.children
  result.children.clear()
  for index in 0..<childCopy.len:
    if index notin toRemove:
      result.children.add childCopy[index]
  

func simplifyOne(e: Expression): Expression {.inline.} =
  e.fixIntPowers()


func simplify*(e: Expression): Expression =
  func f(e: Expression): Expression =
    e.copyMap(f)
    # debugEcho result.children
    result = result.simplifyOne()
  f(e)

func simplify*(f: LinearFormula): LinearFormula =
  result = initLinearFormula()
  for term in f.terms:
    let e = term.e.simplify()
    result.terms.add TermData(
      e: e,
      nonlinearParams: e.paramCount
    )
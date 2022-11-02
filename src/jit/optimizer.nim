import ir {.all.}
from sugar import collect
when isMainModule:
  import ../expressions

func replaceAll*(p: var seq[Command], pattern, replacement: CommandArgument) =
  for c in p.mitems:
    for arg in c.args.mitems:
      if arg == pattern:
        arg = replacement
    if c.result == pattern:
      c.result = replacement
func replaceAll*(p: var seq[CommandArgument], pattern, replacement: CommandArgument) =
  for arg in p.mitems:
    if arg == pattern:
      arg = replacement
func replaceAll*(p: var Program, pattern, replacement: CommandArgument) =
  p.forward.replaceAll(pattern, replacement)
  p.backward.replaceAll(pattern, replacement)
  p.derivatives.replaceAll(pattern, replacement)
  if p.forwardResult == pattern:
    p.forwardResult = replacement

func optimizeConsts(p: var Program) =
  p.replaceAll(constVal(0.0), Zero)
  p.replaceAll(constVal(1.0), One)
  p.replaceAll(constVal(-1.0), MinusOne)

func optimizeMulAdd(
    s: var seq[Command],
    p: var Program) =
  for c in s.mitems:
    if c.kind in {ckAdd, ckMul}:
      let noOpVal = if c.kind == ckAdd: Zero else: One
      if noOpVal in c.args:
        let dest = c.result
        let src =
          if c.args[0] == noOpVal: c.args[1]
          else: c.args[0]
        c = Nop
        p.replaceAll(dest, src)

func optimizeMulAdd(p: var Program) =
  p.forward.optimizeMulAdd(p)
  p.backward.optimizeMulAdd(p)

func optimizeNop(s: seq[Command]): seq[Command] =
  collect(newSeqOfCap(s.len)):
    for c in s:
      if c.kind != ckNop: c

func optimizeNop(p: var Program) =
  p.forward = p.forward.optimizeNop()
  p.backward = p.backward.optimizeNop()

func optimizeUnused(s: var seq[Command], used: var seq[int]) =
  for i in countdown(high(s), low(s)):
    template c: untyped = s[i]
    if c.result.kind == cakIntermediate:
      if c.result.id in used:
        for arg in c.args:
          if arg.kind == cakIntermediate:
            used.add arg.id
      else:
        c = Nop

func optimizeUnused(p: var Program) =
  var used: seq[int]
  if p.forwardResult.kind == cakIntermediate:
    used.add p.forwardResult.id
  for d in p.derivatives:
    if d.kind == cakIntermediate:
      used.add d.id
  p.backward.optimizeUnused(used)
  p.forward.optimizeUnused(used)

func optimize*(p: Program): Program =
  result = p
  result.optimizeConsts()
  result.optimizeMulAdd()
  result.optimizeUnused()
  result.optimizeNop()

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
  echo ""
  echo ""
  let p2 = p.optimize()
  echo p2
  #echo ir.derivatives
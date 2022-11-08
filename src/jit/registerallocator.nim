import ir {.all.}
import tables
import optimizer
from sugar import collect

when isMainModule:
  import ../expressions

const 
  RegisterCount = 8
  NoVar = -1

type Allocator = object
  registers: array[RegisterCount, int]
  registersTemporary: array[RegisterCount, CommandArgument]
  interLifeEnds: Table[int, int]
  memoryIndex: int
  inters: Table[int, CommandArgument]

func bonusLifetime(c: Command, index: int): int =
  if c.kind in {ckIntPower, ckInv} or
     c.kind in {ckDiv, ckSub} and index == 1:
    1
  else:
    0

func findLifetimes(a: var Allocator, s: seq[Command]) =
  var counter = 0
  for c in s:
    for j, arg in c.args:
      if arg.kind == cakIntermediate:
        a.interLifeEnds[arg.id] = counter + bonusLifetime(c, j)
    inc counter

func freeRegisters(a: var Allocator, counter: int) =
  for r in 0..<RegisterCount:
    if a.registers[r] != NoVar:
      let lifetime = a.interLifeEnds[a.registers[r]]
      if lifetime <= counter:
        a.registers[r] = NoVar

func allocateRegister(a: var Allocator, skip: seq[int] = @[]): (CommandArgument, Command) =
  var bestRegister = -1
  var bestRegisterLifetime = -1
  for r in 0..<RegisterCount:
    if r in skip: continue
    if a.registers[r] == NoVar:
      return (
        CommandArgument(kind: cakRegister, id: r),
        Command(kind: ckNop)
      )

    let lifetime = a.interLifeEnds[a.registers[r]]
    if lifetime > bestRegisterLifetime:
      bestRegister = r
      bestRegisterLifetime = lifetime
  let currentVar = a.registers[bestRegister]
  let oldPlace = CommandArgument(kind: cakRegister, id: bestRegister)
  let newPlace = CommandArgument(kind: cakMemory, id: a.memoryIndex)
  inc a.memoryIndex
  a.inters[currentVar] = newPlace
  (oldPlace, Command(
    kind: ckMov,
    args: @[oldPlace],
    result: newPlace
  ))

func allocate(a: var Allocator, s: var seq[Command], counter: var int) =
  var index = 0
  while index < s.len:
    template c: untyped = s[index]
    #if c.kind notin {ckIntPower, ckInv}:
    a.freeRegisters(counter)
    for arg in c.args.mitems:
      if arg.kind == cakIntermediate:
        arg = a.inters[arg.id]
      elif arg.kind != cakZero and arg in a.registersTemporary:
        let i = a.registersTemporary.find(arg)
        arg = CommandArgument(kind: cakRegister, id: i)
    if c.kind in {ckMov, ckPush, ckPop}:
      inc index
      continue
    
    var skipRegisters: seq[int] = @[]
    if c.kind == ckIntPower:
      if c.power == 2:
        c = Command(kind: ckMul, args: @[c.args[0], c.args[0]], result: c.result)
      elif c.args[0].kind != cakRegister:
        let (newRegister, command) = a.allocateRegister()
        if command.kind != ckNop:
          s.insert(command, index)
          inc index
        s.insert(
          Command(kind: ckMov, args: @[c.args[0]], result: newRegister),
          index
        )
        inc index
        skipRegisters.add newRegister.id
        a.registersTemporary[newRegister.id] = c.args[0]
        c.args[0] = newRegister

    if c.result.kind == cakIntermediate:
      let (newRegister, command) = a.allocateRegister(skipRegisters)
      if command.kind != ckNop:
        s.insert(command, index)
        inc index
      a.registers[newRegister.id] = c.result.id
      a.registersTemporary[newRegister.id] = Zero
      a.inters[c.result.id] = newRegister
      c.result = newRegister
    elif c.result.kind in {cakDerivative, cakResult}:
      let (newRegister, command) = a.allocateRegister(skipRegisters)
      if command.kind != ckNop:
        s.insert(command, index)
        inc index
      s.insert(
        Command(kind: ckMov, args: @[newRegister], result: c.result),
        index + 1
      )
      a.registersTemporary[newRegister.id] = c.result
      c.result = newRegister
    
    if c.kind == ckFuncCall:
      let saveRegs = collect(newSeq):
        for r in 0..<RegisterCount:
          if a.registers[r] != NoVar and r != c.result.id:
            CommandArgument(kind: cakRegister, id: r)
      if saveRegs.len > 0:
        s.insert(
          Command(kind: ckPush, args: saveRegs, result: Zero),
          index
        )
        inc index
        s.insert(
          Command(kind: ckPop, args: saveRegs, result: Zero),
          index + 1
        )
    inc index
    inc counter

func allocateDerivs(a: var Allocator, p: var Program) =
  for i, d in p.derivatives:
    if d.kind == cakIntermediate:
      p.replaceAll(d, CommandArgument(kind: cakDerivative, id: i))
  
  if p.forwardResult.kind == cakIntermediate:
    p.replaceAll(p.forwardResult, CommandArgument(kind: cakResult))
  else:
    let (newRegister, command) = a.allocateRegister(@[])
    assert command.kind == ckNop # not sure if this will always work
    p.forward.add Command(
      kind: ckMov,
      args: @[p.forwardResult],
      result: newRegister
    )
    p.forward.add Command(
      kind: ckMov,
      args: @[newRegister],
      result: CommandArgument(kind: cakResult)
    )
    p.forwardResult = CommandArgument(kind: cakResult)

func fixMemoryArgs(s: var seq[Command]) =
  var index = 0
  while index < s.len:
    template c: untyped = s[index]
    if c.kind in {ckAdd, ckMul, ckSub, ckDiv}:
      let firstRegister = c.args[0].kind == cakRegister
      let secondRegister = c.args[1].kind == cakRegister
      if (firstRegister or secondRegister) and c.kind in {ckAdd, ckMul}:
        if not firstRegister:
          swap c.args[0], c.args[1]
      else:
        s.insert(
          Command(kind: ckMov, args: @[c.args[0]], result: c.result),
          index
        )
        inc index
        if c.args[1] == c.args[0]:
          c.args[1] = c.result
        c.args[0] = c.result

      let firstResult = c.args[0] == c.result
      let secondResult = c.args[1] == c.result
      if firstResult or secondResult:
        if not firstResult:
          assert c.kind in {ckAdd, ckMul}
          swap c.args[0], c.args[1]
      else:
        s.insert(
          Command(kind: ckMov, args: @[c.args[0]], result: c.result),
          index
        )
        inc index
        if c.args[1] == c.args[0]:
          c.args[1] = c.result
        c.args[0] = c.result
    elif c.kind == ckFuncCall:
      if c.args[0].id != 0:
        s.insert(
          Command(kind: ckMov, args: c.args,
            result: CommandArgument(kind: cakRegister, id: 0)),
          index
        )
        inc index
        c.args[0].id = 0
      if c.result.id != 0:
        s.insert(
          Command(kind: ckMov, 
            args: @[CommandArgument(kind: cakRegister, id: 0)],
            result: c.result),
          index + 1
        )
        c.result.id = 0
    # elif c.kind in {ckInv, ckFuncCall}:
    #   if c.args[0] != c.result:
    #     s.insert(
    #       Command(kind: ckMov, args: @[c.args[0]], result: c.result),
    #       index
    #     )
    #     inc index
    #     c.args[0] = c.result
    inc index

func fixDerivs(s: var seq[Command], derivs: var seq[CommandArgument]) =
  for i in 0..<derivs.len:
    if derivs[i].kind == cakVariable:
      for index, c in s:
        if c.kind == ckMov and c.args[0] == derivs[i] and c.result.kind == cakRegister:
          derivs[i] = CommandArgument(kind: cakDerivative, id: i)
          s.insert(
            Command(kind: ckMov,
              args: @[c.result],
              result: derivs[i]),
            index+1
          )
          break

func forceFixDerivs(s: var seq[Command], derivs: var seq[CommandArgument]) =
  for i in 0..<derivs.len:
    if derivs[i].kind != cakDerivative:
      s.add Command(kind: ckMov,
        args: @[derivs[i]],
        result: CommandArgument(kind: cakRegister, id: 0)
      )
      derivs[i] = CommandArgument(kind: cakDerivative, id: i)
      s.add Command(kind: ckMov,
        args: @[CommandArgument(kind: cakRegister, id: 0)],
        result: derivs[i]
      )
        


func allocate*(p: Program): Program =
  result = p
  var a = Allocator(
    interLifeEnds: initTable[int, int](),
    memoryIndex: 0,
    inters: initTable[int, CommandArgument](),
  )
  for i in 0..<RegisterCount:
    a.registers[i] = NoVar
    a.registersTemporary[i] = Zero
  
  a.allocateDerivs(result)
  a.findLifetimes(result.forward & result.backward)
  var counter = 0
  a.allocate(result.forward, counter)
  a.allocate(result.backward, counter)

  result.forward.fixMemoryArgs()
  result.backward.fixMemoryArgs()

  result.forward.fixDerivs(result.derivatives)
  result.backward.fixDerivs(result.derivatives)
  result.backward.forceFixDerivs(result.derivatives)
  result.memorySize = a.memoryIndex

func allocateOnlyEval*(p: Program): Program =
  result = p
  var a = Allocator(
    interLifeEnds: initTable[int, int](),
    memoryIndex: 0,
    inters: initTable[int, CommandArgument](),
  )
  for i in 0..<RegisterCount:
    a.registers[i] = NoVar
    a.registersTemporary[i] = Zero
  
  result.replaceAll(result.forwardResult, CommandArgument(kind: cakResult))
  a.findLifetimes(result.forward)
  var counter = 0
  a.allocate(result.forward, counter)

  result.forward.fixMemoryArgs()
  result.memorySize = a.memoryIndex
  result.backward.setLen 0
  result.derivatives.setLen 0

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
      initUnaryExpr(Ln),
      initBigExpr(Sum),
      initBigExpr(Product),
      initVariable(0)
    ),
    initBigExpr(Product).nested(
      initUnaryExpr(Ln),
      initBigExpr(Sum),
      initBigExpr(Product),
      initVariable(1)
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
  echo ""
  echo ""
  let p3 = p2.allocate()
  echo p3
  echo ""
  echo ""
  # let p4 = p2.allocateOnlyEval()
  # echo p4
  #echo ir.derivatives
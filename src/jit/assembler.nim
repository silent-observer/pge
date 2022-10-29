import ir {.all.}
when isMainModule:
  import ../expressions
  import optimizer
  import registerallocator
  from strutils import toHex


# ; void func(double* vars, double* params, double* derivs, double* result, double* consts)
# ;                   rdi           rsi             rdx             rcx             r8/rax
# func:
#     ; if memory used
#     sub RSP, $MemorySize
#     mov RAX, $ConstAddr
#     
#     ...
#     vars[i] = [RDI + i*8]
#     params[i] = [RSI + i*8]
#     derivs[i] = [RDX + i*8]
#     result = [RCX]
#     
#     ...
#     
#     ret

type
  #Assembler = object
  AssembledCode = object
    prog*: seq[byte]
    consts*: seq[float64]

const ConstArrayStart: array[4, float64] = [-0.0, 0.0, 1.0, -1.0]

func calcModRM(reg, rm: CommandArgument): seq[byte] =
  assert reg.kind == cakRegister
  let r = reg.id
  case rm.kind:
  of cakRegister:
    result.add byte(0xC0 or (r shl 3) or rm.id) # ... r, r
  of cakVariable:
    result.add byte(0x47 or (r shl 3)) # ... r, [RDI + i*8]
    result.add byte(rm.id * 8)
  of cakParameter:
    result.add byte(0x46 or (r shl 3)) # ... r, [RSI + i*8]
    result.add byte(rm.id * 8)
  of cakDerivative:
    result.add byte(0x42 or (r shl 3)) # ... r, [RDX + i*8]
    result.add byte(rm.id * 8)
  of cakResult:
    result.add byte(0x01 or (r shl 3)) # ... r, [RCX]
  of cakMemory:
    result.add byte(0x44 or (r shl 3)) # ... r, [ESP + i*8]
    result.add 0x24'u8
    result.add byte(rm.id*8)
  of cakZero, cakOne, cakMinusOne:
    result.add byte(0x40 or (r shl 3)) # ... r, [RAX + i*8]
    if rm.kind == cakZero:
      result.add 8'u8
    elif rm.kind == cakOne:
      result.add 16'u8
    elif rm.kind == cakMinusOne:
      result.add 24'u8
  of cakConst:
    assert false, "Not supported yet"
  of cakIntermediate:
    assert false

func assemble(c: Command, result: var seq[byte]) =
  case c.kind:
  of ckMov:
    if c.result.kind == cakRegister:
      result.add [0xF2'u8, 0x0F, 0x10] # movsd $r, $0
      result.add calcModRM(c.result, c.args[0])
    else:
      result.add [0xF2'u8, 0x0F, 0x11] # movsd $r, $0
      result.add calcModRM(c.args[0], c.result)
  of ckAdd:
    assert c.result == c.args[0]
    result.add [0xF2'u8, 0x0F, 0x58] # addsd $0, $1
    result.add calcModRM(c.args[0], c.args[1])
  of ckSub:
    assert c.result == c.args[0]
    result.add [0xF2'u8, 0x0F, 0x5C] # subsd $0, $1
    result.add calcModRM(c.args[0], c.args[1])
  of ckMul:
    assert c.result == c.args[0]
    if c.args[1] == MinusOne:
      result.add [0x66'u8, 0x0F, 0x57] # xorpd $0, -0.0
      assert c.args[0].kind == cakRegister
      result.add byte(c.args[0].id shl 3)
    else:
      result.add [0xF2'u8, 0x0F, 0x59] # mulsd $0, $1r
      result.add calcModRM(c.args[0], c.args[1])
  of ckInv:
    assert c.result != c.args[0]
    result.add [0xF2'u8, 0x0F, 0x10] # movsd $r, 1
    result.add calcModRM(c.result, One)
    result.add [0xF2'u8, 0x0F, 0x5E] # divsd $r, $0
    result.add calcModRM(c.result, c.args[0])
  of ckLabel, ckFuncCall, ckJump, ckJumpIfSmall, ckIntPower:
    assert false, "Not implemented yet"
  of ckNop, ckDiv:
    assert false

func add32Bit(s: var seq[byte], x: uint32) =
  s.add [
    byte(x and 0xFF),
    byte((x shr 8) and 0xFF),
    byte((x shr 16) and 0xFF),
    byte((x shr 24) and 0xFF)
  ]
func add64Bit(s: var seq[byte], x: uint64) =
  s.add [
    byte(x and 0xFF),
    byte((x shr 8) and 0xFF),
    byte((x shr 16) and 0xFF),
    byte((x shr 24) and 0xFF),
    byte((x shr 32) and 0xFF),
    byte((x shr 40) and 0xFF),
    byte((x shr 48) and 0xFF),
    byte((x shr 56) and 0xFF),
  ]


func assemble*(p: Program): AssembledCode =
  var prelude = @[0x48'u8, 0x81, 0xEC] # sub RSP, X
  prelude.add32Bit(p.memorySize.uint32)
  prelude.add [0x4C'u8, 0x89, 0xC0] # mov RAX, R8

  const Finale = @[0xC3'u8] # ret

  var forwardCode, backwardCode: seq[byte]
  for c in p.forward:
    assemble(c, forwardCode)
  for c in p.backward:
    assemble(c, backwardCode)

  result.prog = prelude & forwardCode & backwardCode & Finale
  result.consts = @ConstArrayStart

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
  echo ""
  echo ""
  let p3 = p2.allocate()
  echo p3
  echo ""
  echo ""

  let code = p3.assemble()
  for b in code.prog:
    stdout.write("0x" & b.toHex() & ", ")
  echo ""
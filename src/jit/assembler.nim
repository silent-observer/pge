import ir {.all.}
from algorithm import reverse
import alignedseq
when isMainModule:
  import ../expressions
  import optimizer
  import registerallocator
  from strutils import toHex


# ; void func(double* vars, double* params, double* derivs, double* result, void* rdata)
# ;                   rdi           rsi             rdx             rcx           r8/rax
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
  Assembler = object
    simd*: SimdCapability
    additionalConsts*: seq[float64]
  AssembledCode = object
    prog*: seq[byte]
    data*: AlignedSeq[byte]
    simd*: SimdCapability

const ConstArrayStart: array[4, float64] = [-0.0, 0.0, 1.0, -1.0]
const ConstArrayAvxStart: array[16, float64] = [
  -0.0, -0.0, -0.0, -0.0,
  0.0, 0.0, 0.0, 0.0,
  1.0, 1.0, 1.0, 1.0,
  -1.0, -1.0, -1.0, -1.0]
const ConstOffset = 0x60'u8

type M256d* {.importc: "__m256d", header: "immintrin.h".} = object

{.compile("avxfuncs.c", "-mavx -O3").}

proc absFunc(x: float64): float64 {.importc: "fabs", header: "<math.h>", cdecl.}
proc expFunc(x: float64): float64 {.importc: "exp", header: "<math.h>", cdecl.}
proc logFunc(x: float64): float64 {.importc: "log", header: "<math.h>", cdecl.}
proc sqrtFunc(x: float64): float64 {.importc: "sqrt", header: "<math.h>", cdecl.}
proc sinFunc(x: float64): float64 {.importc: "sin", header: "<math.h>", cdecl.}
proc cosFunc(x: float64): float64 {.importc: "cos", header: "<math.h>", cdecl.}
proc asinFunc(x: float64): float64 {.importc: "asin", header: "<math.h>", cdecl.}
proc acosFunc(x: float64): float64 {.importc: "acos", header: "<math.h>", cdecl.}
proc atan2Func(y: float64, x: float64): float64 {.importc: "atan2", header: "<math.h>", cdecl.}
proc erfFunc(x: float64): float64 {.importc: "erf", header: "<math.h>", cdecl.}

proc absAvxFunc(x: M256d): M256d {.importc: "fabsAvx", cdecl.}
proc expAvxFunc(x: M256d): M256d {.importc: "expAvx", cdecl.}
proc logAvxFunc(x: M256d): M256d {.importc: "logAvx", cdecl.}
proc sqrtAvxFunc(x: M256d): M256d {.importc: "sqrtAvx", cdecl.}
proc sinAvxFunc(x: M256d): M256d {.importc: "sinAvx", cdecl.}
proc cosAvxFunc(x: M256d): M256d {.importc: "cosAvx", cdecl.}
proc asinAvxFunc(x: M256d): M256d {.importc: "asinAvx", cdecl.}
proc acosAvxFunc(x: M256d): M256d {.importc: "acosAvx", cdecl.}
proc atan2AvxFunc(y: M256d, x: M256d): M256d {.importc: "atan2Avx", cdecl.}
proc erfAvxFunc(x: M256d): M256d {.importc: "erfAvx", cdecl.}

func add32Bit(s: var seq[byte], x: uint32) =
  s.add [
    byte(x and 0xFF),
    byte((x shr 8) and 0xFF),
    byte((x shr 16) and 0xFF),
    byte((x shr 24) and 0xFF)
  ]
func add64Bit(s: var AlignedSeq[byte], x: uint64) =
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

func calcModRMAvx(a: var Assembler, reg, rm: CommandArgument): seq[byte] =
  assert reg.kind == cakRegister
  let r = reg.id
  case rm.kind:
  of cakRegister:
    result.add byte(0xC0 or (r shl 3) or rm.id) # ... r, r
  of cakVariable:
    if rm.id < 4:
      result.add byte(0x47 or (r shl 3)) # ... r, [RDI + i*8*4]
      result.add byte(rm.id * 8 * 4)
    else:
      result.add byte(0x87 or (r shl 3)) # ... r, [RDI + i*8*4]
      result.add32Bit uint32(rm.id * 8 * 4)
  of cakParameter:
    if rm.id < 4:
      result.add byte(0x46 or (r shl 3)) # ... r, [RSI + i*8*4]
      result.add byte(rm.id * 8 * 4)
    else:
      result.add byte(0x86 or (r shl 3)) # ... r, [RDI + i*8*4]
      result.add32Bit uint32(rm.id * 8 * 4)
  of cakDerivative:
    if rm.id < 4:
      result.add byte(0x42 or (r shl 3)) # ... r, [RDX + i*8*4]
      result.add byte(rm.id * 8 * 4)
    else:
      result.add byte(0x82 or (r shl 3)) # ... r, [RDI + i*8*4]
      result.add32Bit uint32(rm.id * 8 * 4)
  of cakResult:
    result.add byte(0x01 or (r shl 3)) # ... r, [RCX]
  of cakMemory:
    if rm.id < 4:
      result.add byte(0x44 or (r shl 3)) # ... r, [ESP + i*8*4]
      result.add 0x24'u8
      result.add byte(rm.id * 8 * 4)
    else:
      result.add byte(0x84 or (r shl 3)) # ... r, [ESP + i*8*4]
      result.add 0x24'u8
      result.add32Bit uint32(rm.id * 8 * 4)
  of cakZero, cakOne, cakMinusOne:
    result.add byte(0x80 or (r shl 3)) # ... r, [RAX + i*32]
    if rm.kind == cakZero:
      result.add [ConstOffset + 0x20, 0x00, 0x00, 0x00]
    elif rm.kind == cakOne:
      result.add [ConstOffset + 0x40, 0x00, 0x00, 0x00]
    elif rm.kind == cakMinusOne:
      result.add [ConstOffset + 0x60, 0x00, 0x00, 0x00]
  of cakConst:
    if rm.c notin a.additionalConsts:
      a.additionalConsts.add rm.c
    let constId = a.additionalConsts.find(rm.c) + 4
    result.add byte(0x80 or (r shl 3)) # ... r, [RAX + i*32]
    let offset = ConstOffset.int + 32 * constId
    result.add32Bit(offset.uint32)
  of cakIntermediate:
    assert false

func calcModRMNoSimd(a: var Assembler, reg, rm: CommandArgument): seq[byte] =
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
      result.add ConstOffset + 0x08
    elif rm.kind == cakOne:
      result.add ConstOffset + 0x10
    elif rm.kind == cakMinusOne:
      result.add ConstOffset + 0x18
  of cakConst:
    if rm.c notin a.additionalConsts:
      a.additionalConsts.add rm.c
    let constId = a.additionalConsts.find(rm.c) + 4
    result.add byte(0x40 or (r shl 3)) # ... r, [RAX + i*8]
    result.add ConstOffset + 8'u8 * byte(constId)
  of cakIntermediate:
    assert false

func calcModRM(a: var Assembler, reg, rm: CommandArgument): seq[byte] =
  case a.simd:
  of NoCapability: a.calcModRMNoSimd(reg, rm)
  of Avx: a.calcModRMAvx(reg, rm)

func assembleAvx(a: var Assembler, c: Command, result: var seq[byte]) =
  template vex660F(vexReg, reg, rm: CommandArgument): array[3, byte] = [
    0xC4'u8,
    0xE1 xor 
      (if reg.id >= 8: 0x80'u8 else: 0x00) xor
      (if rm.kind == cakRegister and rm.id >= 8: 0x20 else: 0x00),
    0x7D xor byte(vexReg.id shl 3)
  ]
  template vex660F(reg, rm: CommandArgument): array[3, byte] =
    vex660F(CommandArgument(kind: cakRegister, id: 0), reg, rm)

  case c.kind:
  of ckMov:
    if c.result.kind == cakRegister:
      result.add vex660F(c.result, c.args[0]) # vmovapd $r, $0
      result.add [0x28'u8]
      result.add a.calcModRM(c.result, c.args[0])
    else:
      result.add vex660F(c.args[0], c.result) # vmovapd $r, $0
      result.add [0x29'u8]
      result.add a.calcModRM(c.args[0], c.result)
  of ckAdd:
    result.add vex660F(c.args[0], c.result, c.args[1]) # vaddpd $r, $0, $1
    result.add [0x58'u8]
    result.add a.calcModRM(c.result, c.args[1])
  of ckSub:
    result.add vex660F(c.args[0], c.result, c.args[1]) # vsubsd $r, $0, $1
    result.add [0x5C'u8]
    result.add a.calcModRM(c.result, c.args[1])
  of ckMul:
    if c.args[1] == MinusOne:
      result.add vex660F(c.args[0], c.args[0], Zero) # vxorpd $0, $0, -0.0
      result.add [0x57'u8]
      assert c.args[0].kind == cakRegister
      result.add byte(0x40 or (c.args[0].id shl 3))
      result.add ConstOffset
    else:
      result.add vex660F(c.args[0], c.result, c.args[1]) # vmulpd $r, $0, $1
      result.add [0x59'u8]
      result.add a.calcModRM(c.result, c.args[1])
  of ckInv:
    assert c.result != c.args[0]
    result.add vex660F(c.result, One) # vmovapd $r, 1
    result.add [0x28'u8]
    result.add a.calcModRM(c.result, One)
    result.add vex660F(c.result, c.result, c.args[0]) # vdivpd $r, $r, $0
    result.add [0x5E'u8]
    result.add a.calcModRM(c.result, c.args[0])
  of ckDiv:
    result.add vex660F(c.args[0], c.result, c.args[1]) # vdivpd $r, $0, $1
    result.add [0x5E'u8]
    result.add a.calcModRM(c.result, c.args[1])
  of ckPush:
    var offset = c.args.len.uint32 * 8 * 4
    result.add [0x48'u8, 0x81, 0xEC] # sub RSP, $args.len*8
    result.add32Bit(offset)
    for i, arg in c.args:
      result.add vex660F(arg, CommandArgument(kind: cakMemory, id: i)) # vmovupd [RSP+offset], $arg
      result.add [0x11'u8]
      result.add a.calcModRM(arg, CommandArgument(kind: cakMemory, id: i))
  of ckPop:
    for i, arg in c.args:
      result.add vex660F(arg, CommandArgument(kind: cakMemory, id: i)) # vmovupd $arg, [RSP+offset]
      result.add [0x10'u8]
      result.add a.calcModRM(arg, CommandArgument(kind: cakMemory, id: i))
    var offset = c.args.len.uint32 * 8 * 4
    result.add [0x48'u8, 0x81, 0xC4] # add RSP, $args.len*8
    result.add32Bit(offset)
  of ckFuncCall:
    assert c.result == c.args[0]
    assert c.result.kind == cakRegister and c.result.id == 0
    result.add [
      0x50'u8, 0x51, 0x52, 0x56, 0x57, # push RAX, RCX, RDX, RSI, RDI
      0xFF, 0x50 # mov RAX, $address
    ]
    case c.funcName:
    of "abs": result.add 0x00'u8
    of "exp": result.add 0x08'u8
    of "log": result.add 0x10'u8
    of "sqrt": result.add 0x18'u8
    of "sin": result.add 0x20'u8
    of "cos": result.add 0x28'u8
    of "asin": result.add 0x30'u8
    of "acos": result.add 0x38'u8
    of "atan2": result.add 0x40'u8
    of "erf": result.add 0x48'u8
    else: assert false, "No such function " & c.funcName
    result.add [
      0x5F'u8, 0x5E, 0x5A, 0x59, 0x58, # pop RAX, RCX, RDX, RSI, RDI
    ]
  of ckIntPower:
    var power = c.power
    var binary: seq[int] = @[]
    while power > 0:
      binary.add(power mod 2)
      power = power div 2
    binary.reverse()
    
    result.add vex660F(c.result, c.args[0]) # vmovapd $result, $arg
    result.add [0x28'u8]
    result.add a.calcModRM(c.result, c.args[0])
    for b in binary[1..^1]:
      result.add vex660F(c.result, c.result, c.result) # vmulpd $result, $result
      result.add [0x59'u8]
      result.add a.calcModRM(c.result, c.result)
      if b == 1:
        result.add vex660F(c.result, c.result, c.args[0]) # vmulpd $result, $arg
        result.add [0x59'u8]
        result.add a.calcModRM(c.result, c.args[0])

  of ckNop:
    assert false


func assembleNoSimd(a: var Assembler, c: Command, result: var seq[byte]) =
  case c.kind:
  of ckMov:
    if c.result.kind == cakRegister:
      result.add [0xF2'u8, 0x0F, 0x10] # movsd $r, $0
      result.add a.calcModRM(c.result, c.args[0])
    else:
      result.add [0xF2'u8, 0x0F, 0x11] # movsd $r, $0
      result.add a.calcModRM(c.args[0], c.result)
  of ckAdd:
    assert c.result == c.args[0]
    result.add [0xF2'u8, 0x0F, 0x58] # addsd $0, $1
    result.add a.calcModRM(c.args[0], c.args[1])
  of ckSub:
    assert c.result == c.args[0]
    result.add [0xF2'u8, 0x0F, 0x5C] # subsd $0, $1
    result.add a.calcModRM(c.args[0], c.args[1])
  of ckMul:
    assert c.result == c.args[0]
    if c.args[1] == MinusOne:
      result.add [0x66'u8, 0x0F, 0x57] # xorpd $0, -0.0
      assert c.args[0].kind == cakRegister
      result.add byte(0x40 or (c.args[0].id shl 3))
      result.add ConstOffset
    else:
      result.add [0xF2'u8, 0x0F, 0x59] # mulsd $0, $1r
      result.add a.calcModRM(c.args[0], c.args[1])
  of ckInv:
    assert c.result != c.args[0]
    result.add [0xF2'u8, 0x0F, 0x10] # movsd $r, 1
    result.add a.calcModRM(c.result, One)
    result.add [0xF2'u8, 0x0F, 0x5E] # divsd $r, $0
    result.add a.calcModRM(c.result, c.args[0])
  of ckDiv:
    assert c.result == c.args[0]
    result.add [0xF2'u8, 0x0F, 0x5E] # divsd $r, $0
    result.add a.calcModRM(c.args[0], c.args[1])
  of ckPush:
    var offset = c.args.len.uint32 * 8
    if offset mod 16 != 0:
      offset += 8
    result.add [0x48'u8, 0x81, 0xEC] # sub RSP, $args.len*8
    result.add32Bit(offset)
    for i, arg in c.args:
      result.add [0xF2'u8, 0x0F, 0x11] # movsd [RSP+offset], $arg
      result.add a.calcModRM(arg, CommandArgument(kind: cakMemory, id: i))
  of ckPop:
    for i, arg in c.args:
      result.add [0xF2'u8, 0x0F, 0x10] # movsd $arg, [RSP+offset]
      result.add a.calcModRM(arg, CommandArgument(kind: cakMemory, id: i))
    var offset = c.args.len.uint32 * 8
    if offset mod 16 != 0:
      offset += 8
    result.add [0x48'u8, 0x81, 0xC4] # add RSP, $args.len*8
    result.add32Bit(offset)
  of ckFuncCall:
    assert c.result == c.args[0]
    assert c.result.kind == cakRegister and c.result.id == 0
    result.add [
      0x50'u8, 0x51, 0x52, 0x56, 0x57, # push RAX, RCX, RDX, RSI, RDI
      0xFF, 0x50 # mov RAX, $address
    ]
    case c.funcName:
    of "abs": result.add 0x00'u8
    of "exp": result.add 0x08'u8
    of "log": result.add 0x10'u8
    of "sqrt": result.add 0x18'u8
    of "sin": result.add 0x20'u8
    of "cos": result.add 0x28'u8
    of "asin": result.add 0x30'u8
    of "acos": result.add 0x38'u8
    of "atan2": result.add 0x40'u8
    of "erf": result.add 0x48'u8
    else: assert false, "No such function " & c.funcName
    result.add [
      0x5F'u8, 0x5E, 0x5A, 0x59, 0x58, # pop RAX, RCX, RDX, RSI, RDI
    ]
  of ckIntPower:
    var power = c.power
    var binary: seq[int] = @[]
    while power > 0:
      binary.add(power mod 2)
      power = power div 2
    binary.reverse()
    
    result.add [0xF2'u8, 0x0F, 0x10] # movsd $result, $arg
    result.add a.calcModRM(c.result, c.args[0])
    for b in binary[1..^1]:
      result.add [0xF2'u8, 0x0F, 0x59] # mulsd $result, $result
      result.add a.calcModRM(c.result, c.result)
      if b == 1:
        result.add [0xF2'u8, 0x0F, 0x59] # mulsd $result, $arg
        result.add a.calcModRM(c.result, c.args[0])

  of ckNop:
    assert false

func assemble(a: var Assembler, c: Command, result: var seq[byte]) =
  case a.simd:
  of NoCapability: a.assembleNoSimd(c, result)
  of Avx: a.assembleAvx(c, result)

func assemble*(p: Program, simd: SimdCapability): AssembledCode =
  var prelude = @[0x48'u8, 0x81, 0xEC] # sub RSP, X
  prelude.add32Bit(p.memorySize.uint32)
  prelude.add [0x4C'u8, 0x89, 0xC0] # mov RAX, R8

  const Finale = @[0xC3'u8] # ret

  var a = Assembler(simd: simd)

  var forwardCode, backwardCode: seq[byte]
  for c in p.forward:
    a.assemble(c, forwardCode)
  for c in p.backward:
    a.assemble(c, backwardCode)

  result.prog = prelude & forwardCode & backwardCode & Finale
  let consts = case simd:
    of NoCapability: @ConstArrayStart & a.additionalConsts
    of Avx: @ConstArrayAvxStart & a.additionalConsts
  let funcs = case simd:
    of NoCapability: [
      cast[uint64](absFunc),
      cast[uint64](expFunc),
      cast[uint64](logFunc),
      cast[uint64](sqrtFunc),
      cast[uint64](sinFunc),
      cast[uint64](cosFunc),
      cast[uint64](asinFunc),
      cast[uint64](acosFunc),
      cast[uint64](atan2Func),
      cast[uint64](erfFunc),
      0,
      0
    ]
    of Avx: [
      cast[uint64](absAvxFunc),
      cast[uint64](expAvxFunc),
      cast[uint64](logAvxFunc),
      cast[uint64](sqrtAvxFunc),
      cast[uint64](sinAvxFunc),
      cast[uint64](cosAvxFunc),
      cast[uint64](asinAvxFunc),
      cast[uint64](acosAvxFunc),
      cast[uint64](atan2AvxFunc),
      cast[uint64](erfAvxFunc),
      0,
      0
    ]
  for f in funcs:
    result.data.add64Bit f
  for c in consts:
    result.data.add64Bit cast[uint64](c)

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
  # let e = initBigExpr(Sum).nested(
  #   initBigExpr(Product),
  #   initUnaryExpr(Asin),
  #   initBigExpr(Sum),
  #   initBigExpr(Product),
  #   initVariable(0)
  # )
  let e = initBigExpr(Sum).withChildren(
    initBigExpr(Product).nested(
      initIntPower(10),
      initVariable(0)
    ),
    initBigExpr(Product).nested(
      initIntPower(5),
      initUnaryExpr(Sin),
      initBigExpr(Product),
      initVariable(0)
    ),
    initBigExpr(Product).nested(
      initIntPower(2),
      initVariable(0)
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
  let p3 = p2.allocate(NoCapability)
  echo p3
  echo ""
  echo ""

  let code = p3.assemble(NoCapability)
  for b in code.prog:
    stdout.write("" & b.toHex() & " ")
  echo ""
  echo ""
  # for b in code.data:
  #   stdout.write("" & b.toHex() & " ")
  # echo ""
import matrix, variabledata
import expressions, formula, paramgenerator
import jit/jit
import sugar
from fenv import epsilon
from math import sum, isNaN, exp
#import nimprof
from algorithm import fill
from sequtils import map, toSeq
#from macros import expandMacro

const EvalJit = true

proc fillData(f: LinearFormula,
    programs: seq[JitProgram],
    vars: VariableData,
    nlParams, y: Vector,
    n, varCount, p, pl: int,
    jacobian: var Matrix,
    linearParams, errors, fVals: var Vector): Number =
  var phi = matrix(n, pl)
  var derivs = matrix(n, p)
  
  var derivsRow = vector(p)
  # var testDerivsRow = vector(p)
  for i in 0..<n:
    derivsRow.data.fill(0.0)
    # testDerivsRow.data.fill(0.0)
    var paramIndex = 0
    for k in 0..<pl-1:
      # debugEcho vars[i, _].squeeze(0)
      when EvalJit:
        let val = programs[k].evalAll(vars[i], nlParams, paramIndex, derivsRow)
        if isNaN(val) or abs(val) > 1e10:
          # debugEcho "NaNs!"
          return Inf

        phi[i, k] = val
        paramIndex += f.terms[k].nonlinearParams

        # let startParamIndex = paramIndex
        # let newVal = f.terms[k].e.eval(vars[i], nlParams, paramIndex)
        # paramIndex = startParamIndex
        # f.terms[k].e.evalDerivs(vars[i], nlParams, paramIndex, testDerivsRow)
        # if abs(newVal) > 1e-10 and norm(derivsRow) > 1e-10 and
        #     (abs(val - newVal)/abs(newVal) > 1e-5 or 
        #     norm(derivsRow - testDerivsRow)/norm(derivsRow) > 1e-5):
        #   debugEcho f.terms[k].e
        #   debugEcho vars[i][0]
        #   debugEcho nlParams
        #   debugEcho val
        #   debugEcho newVal
        #   debugEcho derivsRow
        #   debugEcho testDerivsRow
        #   quit(0)
      else:
        let startParamIndex = paramIndex
        let val = f.terms[k].e.eval(vars[i], nlParams, paramIndex)
        if isNaN(val) or abs(val) > 1e10:
          # debugEcho "NaNs!"
          return Inf

        phi[i, k] = val
        # debugEcho val
        # debugEcho phi[i, k]
        paramIndex = startParamIndex
        f.terms[k].e.evalDerivs(vars[i], nlParams, paramIndex, derivsRow)
    # debugEcho derivsRow
    derivs.setRow(derivsRow, i)
    
    phi[i, pl-1] = 1
  # for i in 0..<n:
  #   var paramIndex = 0
  #   for k in 0..<pl-1:
  #     # debugEcho vars[i, _].squeeze(0)
  #     let startParamIndex = paramIndex
  #     let val = f.terms[k].eval(vars[i, _].squeeze(0), nlParams, paramIndex)
  #     if isNaN(val) or abs(val) > 1e10:
  #       # debugEcho "NaNs!"
  #       raise newException(ValueError, "NaNs in the data.")

  #     phi[i, k] = val
  #     # debugEcho phi[i, k]
  #     paramIndex = startParamIndex
  #     derivs[i, _] = 
  #       f.terms[k].evalDerivs(vars[i, _].squeeze(0), 
  #         nlParams, paramIndex, derivs[i, _].squeeze(0)).unsqueeze(0)
    
  #   phi[i, pl-1] = 1
  
  # debugEcho "Phi = ", phi
  # debugEcho "derivs = ", derivs
  
  let decomp = phi.svd()
  let (u, sigma, vh) = (decomp.u, decomp.sigma, decomp.vt)

  # debugEcho "u = ", u
  # debugEcho "sigma = ", sigma
  # debugEcho "vh = ", vh

  let sigmaMax = sigma.Vector[0]
  if sigmaMax == Inf: 
    return Inf
  let sigmaMin = max(1.0, sigmaMax * n.Number) * epsilon(Number)
  let sigmaInv = sigma
    .Vector
    .data
    .map(x => (if x < sigmaMin: 0.0 else: 1.0/x))
    .vector
    .DiagonalMatrix

  # debugEcho "uTy = ", u.transpose * y
  # debugEcho "SuTy = ", sigmaInv *. (u.transpose * y)

  #expandMacros:
  let c = vh ^* (sigmaInv * (u ^* y))

# debugEcho "c = ", c

  var kFun = newSeq[int](p)
  var cs = vector(p)
  var paramIndex = 0
  for k in 0..<pl-1:
    let paramCount = f.terms[k].nonlinearParams
    if paramCount == 0: continue
    for s in paramIndex..<paramIndex+paramCount:
      kFun[s] = k
      cs[s] = c[k]
    paramIndex += paramCount
  
  # debugEcho "kFun = ", kFun

  fVals = phi * c
  let r = y - fVals
  result = r.norm()

  # debugEcho "r = ", r

  let dc = derivs * DiagonalMatrix(cs)
  let drVec = derivs ^* r
  var dr = matrix(pl, p)

  for s in 0..<p:
    dr[kFun[s], s] = drVec[s]
  # debugEcho "dc = ", dc
  # debugEcho "dr = ", dr

  let uudc = u * (u ^* dc)
  let a = dc - uudc

  # debugEcho "dr: ", dr
  # debugEcho "vh: ", vh
  # debugEcho "sigmaInv: ", sigmaInv
  # debugEcho "u: ", u
  let b = u * (sigmaInv * (vh * dr))

  # debugEcho "uudc = ", uudc
  # debugEcho "a = ", a
  # debugEcho "b = ", b

  linearParams = c
  jacobian = -(a + b)
  errors = r
  # debugEcho "jacobian = ", jacobian

proc evalOnly(f: LinearFormula,
    programs: seq[JitProgram],
    vars: VariableData,
    nlParams, y: Vector,
    n, varCount, p, pl: int, linearParams: var Vector): Vector =
  var phi = matrix(n, pl)
  # var derivs = zeros[Number](n, p)
  for i in 0..<n:
    var paramIndex = 0
    for k in 0..<pl-1:
      when EvalJit:
        let val = programs[k].eval(vars[i], paramIndex, nlParams)
        if isNaN(val) or abs(val) > 1e10:
          # debugEcho "NaNs!"
          return vector(0)
        phi[i, k] = val
        paramIndex += f.terms[k].nonlinearParams
      else:
      # debugEcho vars[i, _].squeeze(0)
      # let startParamIndex = paramIndex
        let val = f.terms[k].e.eval(vars[i], nlParams, paramIndex)
        if isNaN(val) or abs(val) > 1e10:
          # debugEcho "NaNs!"
          return vector(0)
        phi[i, k] = val
      # debugEcho phi[i, k]
    phi[i, pl-1] = 1
  
  # debugEcho "Phi = ", phi
  # debugEcho "derivs = ", derivs
  
  let decomp = phi.svd()
  let (u, sigma, vh) = (decomp.u, decomp.sigma, decomp.vt)

  # debugEcho "u = ", u
  # debugEcho "sigma = ", sigma
  # debugEcho "vh = ", vh

  let sigmaMax = sigma.Vector[0]
  if sigmaMax == Inf: return vector(0)
  let sigmaMin = max(1.0, sigmaMax * n.Number) * epsilon(Number)
  let sigmaInv = sigma
    .Vector
    .data
    .map(x => (if x < sigmaMin: 0.0 else: 1.0/x))
    .vector
    .DiagonalMatrix

  # debugEcho "sigmaInv = ", sigmaInv
  # debugEcho "uTy = ", u.transpose * y
  # debugEcho "SuTy = ", sigmaInv *. (u.transpose * y)

  linearParams = vh ^* (sigmaInv * (u ^* y))

  # debugEcho "c = ", c

  result = phi * linearParams

# func choleskyDecompose(a: var Tensor[Number]) =
#   assert a.rank == 2, "You can only decompose a matrix"
#   assert a.shape[0] == a.shape[1], "You can only decompose a square matrix"

#   let n = a.shape[0]
#   for i in 0..<n:
#     for j in 0..i:
#       var s = 0.0
#       for k in 0..<j:
#         s += a[i, k] * a[j, k]
#       if i == j:
#         a[i, j] = sqrt(a[i, j] - s)
#       else:
#         a[i, j] = (a[i, j] - s) / a[j, j]
#     for j in i+1..<n:
#       a[i, j] = 0

# proc choleskySolve(a, v: Tensor[Number]): Tensor[Number] =
#   assert a.rank == 2, "You can only decompose a matrix"
#   assert a.shape[0] == a.shape[1], "You can only decompose a square matrix"
#   let n = a.shape[0]

#   assert v.rank == 1, "The right hand side must be a vector"
#   assert v.shape[0] == a.shape[1], "You can only decompose a square matrix"

#   var y = newTensorUninit[Number](n)
#   for i in 0..<n:
#     var num = v[i]
#     for j in 0..<i:
#       num -= y[j] * a[i, j]
#     y[i] = num / a[i, i]

#   result = newTensorUninit[Number](n)
#   for i in 0..<n:
#     var num = y[i]
#     for j in 0..<i:
#       num -= result[j] * a[j, i]
#     result[i] = num / a[i, i]

proc fitParams(f: LinearFormula,
    programs: seq[JitProgram],
    vars: VariableData,
    y: Vector,
    startParams: Vector):
    tuple[linearParams, nonlinearParams: Vector, error: Number] =
  let (n, varCount) = (vars.rows, vars.varCount)
  let p = startParams.len
  let pl = f.terms.len + 1

  var
    jacobian: Matrix
    linearParams, errors, fVals: Vector
  var
    newJacobian: Matrix
    newLinearParams, newErrors, newFVals: Vector

  var params = startParams
  # echo params
  var err = f.fillData(programs, vars, params, y, n, varCount, p, pl, jacobian, linearParams, errors, fVals)
  # debugEcho err
  # debugEcho jacobian
  # debugEcho linearParams
  # debugEcho "errors = ", errors
  if isNaN(err) or err == Inf:
    return (
      linearParams: vector(0), 
      nonlinearParams: vector(0),
      error: Inf)
  var lambda = 100.0

  var consecutiveFail = 0
  var step = 0

  while step < 50:
    var jj = jacobian.gramMatrix()
    for i in 0..<jj.Matrix.rows:
      jj.Matrix[i, i] *= 1.0 + lambda
    let grad = jacobian ^* errors
    # debugEcho grad

    let jjTriangular = jj.choleskyDecompose()

    var delta = -jjTriangular.choleskySolve(grad)
    # debugEcho delta
    # echo delta
    # echo delta
    # echo params, " ", err
    const GravityEnable = false
    when GravityEnable:
      const hStep = 0.1

      var paramsAfterStep = params + hStep * delta
      var cAfterStep: Vector
      let fAfterStep = f.evalOnly(programs, vars, paramsAfterStep, y, n, varCount, p, pl, cAfterStep)
      if fAfterStep.len == 0:
        return (
          linearParams: vector(0), 
          nonlinearParams: vector(0),
          error: Inf
        )
      
      let kVec = (2.0 / hStep) * ((1.0 / hStep) * (fAfterStep - fVals) - jacobian * delta)
      let accel = -jjTriangular.choleskySolve(jacobian ^* kVec)

      const alpha = 0.75
      if 4.0 * dot(accel, accel) / dot(delta, delta) <= alpha * alpha:
        delta += 0.5 * accel
    # debugEcho delta
    # echo abs(delta).max()
    if absMax(delta) < 1e-10: break
    # echo "!!!"

    var newParams = params + delta
    #echo "P ", params
    var newErr = f.fillData(programs, vars, newParams, y, n, varCount, p, pl, newJacobian, newLinearParams, newErrors, newFVals)

    # echo "-> ", newParams, " ", newErr

    if newErr < 1e-9: break
    if newErr < err:
      params = newParams
      # echo "-> ", params
      jacobian = newJacobian
      linearParams = newLinearParams
      errors = newErrors
      fVals = newFVals
      err = newErr

      if lambda > 1e-14:
        lambda /= 3
      consecutiveFail = 0
      inc step
    else:
      if lambda < 1e14:
        lambda *= 2
      inc consecutiveFail
      if consecutiveFail >= 10:
        inc step
  (linearParams: linearParams, nonlinearParams: params, error: err)

proc fitParams*(f: LinearFormula, vars: VariableData, y: Vector, howMany = 30):
    tuple[linearParams, nonlinearParams: Vector, error: Number] =
  var paramCount = 0
  for term in f.terms:
    paramCount += term.nonlinearParams

  var programs = collect(newSeqOfCap(f.terms.len)):
    for term in f.terms:
      term.e.compile()

  if paramCount == 0:
    let (n, varCount) = (vars.rows, vars.varCount)
    let pl = f.terms.len + 1
    let fVals = f.evalOnly(programs, vars, vector(0), y,
      n, varCount, 0, pl, result.linearParams)
    if fVals.len == 0:
      result.error = Inf
    else:
      result.error = norm(fVals - y)
    return
  else:
    result.error = Inf
    var counter = 0
    for params in generateInitialParams(paramCount, howMany):
      # debugEcho "!", counter, " ", params
      inc counter
      let t = f.fitParams(programs, vars, y, params)
      # echo t.error, " -> ", params
      if t.error < result.error:
        result = t
        if result.error < 1e-7: break

if isMainModule:
  # disableProfiling()
  let x = @[-2.0, -1.0, 0.0, 1.0, 2.0]
  let vars = toVariableData(@[@[-2.0], @[-1.0], @[0.0], @[1.0], @[2.0]])
  let y = x.map(x => exp(1 * x) + exp(-1 * x)).vector
  echo "x = ", x
  echo "vars = ", vars
  echo "y = ", y

  let f = initLinearFormula(
    initBigExpr(Product, constDisabled=true).nested(
      initUnaryExpr(ExprKind.Exp),
      initBigExpr(Product),
      initVariable(0)
    ),
    initBigExpr(Product, constDisabled=true).nested(
      initUnaryExpr(ExprKind.Exp),
      initBigExpr(Product),
      initVariable(0)
    )
  )
  echo f

  # setSamplingFrequency(20000)
  # enableProfiling()
  #let startTime = getMonoTime()
  #for i in 0..<1000:
  # if i mod 100 == 0:
  #   echo i
  let t = f.fitParams(vars, y)
  echo t
  #echo getMonoTime() - startTime
  #echo t
  
  #var
  #  jacobian = newTensorUninit[Number](5, 2)
  #  linearParams = newTensorUninit[Number](3)
  #f.fillData(vars, startParams, y, n, varCount, p, pl, jacobian, linearParams)
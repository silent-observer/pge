import arraymancer
import expressions, formula, paramgenerator
import sugar
from fenv import epsilon
from math import sum, isNaN
#import nimprof
import times, std/monotimes
from algorithm import fill
from sequtils import map, toSeq
#from macros import expandMacros

proc part1(f: LinearFormula, 
    vars: seq[seq[Number]],
    nlParams, y: seq[Number],
    n, varCount, p, pl: int,
    phi, derivs: var Tensor) =
  var derivsRow = newSeq[Number](p)
  for i in 0..<n:
    derivsRow.fill(0.0)
    var paramIndex = 0
    for k in 0..<pl-1:
      # debugEcho vars[i, _].squeeze(0)
      let startParamIndex = paramIndex
      let val = f.terms[k].eval(vars[i], nlParams, paramIndex)
      if isNaN(val) or abs(val) > 1e10:
        # debugEcho "NaNs!"
        raise newException(ValueError, "NaNs in the data.")

      phi[i, k] = val
      # debugEcho phi[i, k]
      paramIndex = startParamIndex
      f.terms[k].evalDerivs(vars[i], nlParams, paramIndex, derivsRow)
    derivs[i, _] = derivsRow.toTensor().reshape(1, p)
    
    phi[i, pl-1] = 1

proc fillData(f: LinearFormula, 
    vars: seq[seq[Number]],
    nlParams, y: seq[Number],
    n, varCount, p, pl: int,
    jacobian, linearParams, errors, fVals: var Tensor[Number]): Number =
  var phi = zeros[Number](n, pl)
  var derivs = zeros[Number](n, p)
  part1(f, vars, nlParams, y, n, varCount, p, pl, phi, derivs)
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
  let (u, sigma, vh) = (decomp.U, decomp.S, decomp.Vh)

  # debugEcho "u = ", u
  # debugEcho "sigma = ", sigma
  # debugEcho "vh = ", vh

  let sigmaMax = sigma[0]
  let sigmaMin = sigmaMax * n.Number * epsilon(Number)
  let sigmaInv = sigma.map(x => (if x < sigmaMin: 0.0 else: 1.0/x))

  # debugEcho "sigmaInv = ", sigmaInv
  # debugEcho "uTy = ", u.transpose * y
  # debugEcho "SuTy = ", sigmaInv *. (u.transpose * y)

  #expandMacros:
  let yTensor = y.toTensor()
  let c = vh.transpose * (sigmaInv *. (u.transpose * yTensor))

# debugEcho "c = ", c

  var kFun = newSeq[int](p)
  var paramIndex = 0
  for k in 0..<pl-1:
    let paramCount = f.nonlinearParams[k]
    if paramCount == 0: continue
    for i in paramIndex..<paramIndex+paramCount:
      kFun[i] = k
    paramIndex += paramCount
  
  # debugEcho "kFun = ", kFun

  fVals = phi * c
  let r = yTensor - fVals
  result = fVals.mean_squared_error(yTensor)

  # debugEcho "r = ", r

  var dc = newTensorUninit[Number](n, p)
  var dr = zeros[Number](pl, p)

  for s in 0..<p:
    dc[_, s] = derivs[_, s] * c[kFun[s]]
    dr[kFun[s], s] = dot(r, derivs[_,s].squeeze(1))
  # debugEcho "dc = ", dc
  # debugEcho "dr = ", dr

  let uudc = u * u.transpose * dc
  let a = dc - uudc

  let b = u * (sigmaInv.unsqueeze(1) *. (vh * dr))

  # debugEcho "uudc = ", uudc
  # debugEcho "a = ", a
  # debugEcho "b = ", b

  linearParams = c
  jacobian = -(a + b)
  errors = r
  # debugEcho "jacobian = ", jacobian

proc evalOnly(f: LinearFormula,
    vars: seq[seq[Number]],
    nlParams, y: seq[Number],
    n, varCount, p, pl: int, linearParams: var Tensor[Number]): Tensor[Number] =
  var phi = zeros[Number](n, pl)
  # var derivs = zeros[Number](n, p)
  for i in 0..<n:
    var paramIndex = 0
    for k in 0..<pl-1:
      # debugEcho vars[i, _].squeeze(0)
      # let startParamIndex = paramIndex
      let val = f.terms[k].eval(vars[i], nlParams, paramIndex)
      if isNaN(val) or abs(val) > 1e10:
        # debugEcho "NaNs!"
        raise newException(ValueError, "NaNs in the data.")
      phi[i, k] = val
      # debugEcho phi[i, k]
    phi[i, pl-1] = 1
  
  # debugEcho "Phi = ", phi
  # debugEcho "derivs = ", derivs
  
  let decomp = phi.svd()
  let (u, sigma, vh) = (decomp.U, decomp.S, decomp.Vh)

  # debugEcho "u = ", u
  # debugEcho "sigma = ", sigma
  # debugEcho "vh = ", vh

  let sigmaMax = sigma[0]
  let sigmaMin = sigmaMax * n.Number * epsilon(Number)
  let sigmaInv = sigma.map(x => (if x < sigmaMin: 0.0 else: 1.0/x))

  # debugEcho "sigmaInv = ", sigmaInv
  # debugEcho "uTy = ", u.transpose * y
  # debugEcho "SuTy = ", sigmaInv *. (u.transpose * y)

  linearParams = vh.transpose * (sigmaInv *. (u.transpose * y.toTensor()))

  # debugEcho "c = ", c

  result = phi * linearParams

func choleskyDecompose(a: var Tensor[Number]) =
  assert a.rank == 2, "You can only decompose a matrix"
  assert a.shape[0] == a.shape[1], "You can only decompose a square matrix"

  let n = a.shape[0]
  for i in 0..<n:
    for j in 0..i:
      var s = 0.0
      for k in 0..<j:
        s += a[i, k] * a[j, k]
      if i == j:
        a[i, j] = sqrt(a[i, j] - s)
      else:
        a[i, j] = (a[i, j] - s) / a[j, j]
    for j in i+1..<n:
      a[i, j] = 0

proc choleskySolve(a, v: Tensor[Number]): Tensor[Number] =
  assert a.rank == 2, "You can only decompose a matrix"
  assert a.shape[0] == a.shape[1], "You can only decompose a square matrix"
  let n = a.shape[0]

  assert v.rank == 1, "The right hand side must be a vector"
  assert v.shape[0] == a.shape[1], "You can only decompose a square matrix"

  var y = newTensorUninit[Number](n)
  for i in 0..<n:
    var num = v[i]
    for j in 0..<i:
      num -= y[j] * a[i, j]
    y[i] = num / a[i, i]

  result = newTensorUninit[Number](n)
  for i in 0..<n:
    var num = y[i]
    for j in 0..<i:
      num -= result[j] * a[j, i]
    result[i] = num / a[i, i]

proc fitParams(f: LinearFormula,
    vars: seq[seq[Number]],
    y: seq[Number],
    startParams: seq[Number]):
    tuple[linearParams, nonlinearParams: seq[Number], error: Number] =
  let (n, varCount) = (vars.len, vars[0].len)
  let p = startParams.len
  let pl = f.terms.len + 1

  var jacobian, linearParams, errors, fVals: Tensor[Number]
  var newJacobian, newLinearParams, newErrors, newFVals: Tensor[Number]

  var params = startParams
  # echo params
  var err = f.fillData(vars, params, y, n, varCount, p, pl, jacobian, linearParams, errors, fVals)
  var lambda = 100.0

  var consecutiveFail = 0
  var step = 0

  while step < 50:
    var jj = jacobian.transpose * jacobian
    for i in 0..<jj.shape[0]:
      jj[i, i] *= 1.0 + lambda
    let grad = jacobian.transpose * errors

    jj.choleskyDecompose()

    var delta = -jj.choleskySolve(grad)
    # echo delta
    # echo delta
    # echo params, " ", err

    const hStep = 0.1

    var paramsAfterStep = params
    #echo params
    for i in 0..<p:
      paramsAfterStep[i] += hStep * delta[i]
    var cAfterStep: Tensor[Number]
    let fAfterStep = f.evalOnly(vars, paramsAfterStep, y, n, varCount, p, pl, cAfterStep)
    let kVec = (2.0 / hStep) * ((1.0 / hStep) * (fAfterStep - fVals) - jacobian * delta)
    let accel = -jj.choleskySolve(jacobian.transpose * kVec)

    const alpha = 0.75
    if 4.0 * dot(accel, accel) / dot(delta, delta) <= alpha * alpha:
      delta += 0.5 * accel

    # echo abs(delta).max()
    if abs(delta).max() < 1e-8: break
    # echo "!!!"

    var newParams = newSeq[Number](params.len)
    for i in 0..<p:
      newParams[i] = params[i] + delta[i]
    #echo "P ", params
    var newErr = f.fillData(vars, newParams, y, n, varCount, p, pl, newJacobian, newLinearParams, newErrors, newFVals)
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
  (linearParams: linearParams.toSeq1D(), nonlinearParams: params, error: err)

proc fitParams*(f: LinearFormula, vars: seq[seq[Number]], y: seq[Number]):
    tuple[linearParams, nonlinearParams: seq[Number], error: Number] =
  let paramCount = f.nonlinearParams.sum()
  var lParams: Tensor[Number]
  if paramCount == 0:
    let (n, varCount) = (vars.len, vars[0].len)
    let pl = f.terms.len + 1
    try:
      let fVals = f.evalOnly(vars, @[], y,
        n, varCount, 0, pl, lParams)
      result.error = fVals.mean_squared_error(y.toTensor())
      result.linearParams = lParams.toSeq1D()
    except ValueError:
      result.error = Inf
    return
  else:
    result.error = Inf
    var counter = 0
    for params in generateInitialParams(paramCount):
      # debugEcho "!", counter, " ", params
      inc counter
      try:
        let t = f.fitParams(vars, y, params)
        # echo t.error, " -> ", params
        if t.error < result.error:
          result = t
          if result.error < 1e-7: break
      except ValueError:
        discard
      except:
        echo f
        raise getCurrentException()

if isMainModule:
  # disableProfiling()
  let x = @[-2.0, -1.0, 0.0, 1.0, 2.0]
  let vars = @[@[-2.0], @[-1.0], @[0.0], @[1.0], @[2.0]]
  let y = x.map(x => exp(1 * x) + exp(-1 * x)).toSeq()
  echo "x = ", x
  echo "vars = ", vars
  echo "y = ", y

  let f = initBigExpr(Sum).withChildren(
    initBigExpr(Product).nested(
      initUnaryExpr(ExprKind.Exp),
      initBigExpr(Product),
      initVariable(0)
    ),
    initBigExpr(Product).nested(
      initUnaryExpr(ExprKind.Exp),
      initBigExpr(Product),
      initVariable(0)
    )
  ).linearize()
  echo f

  # setSamplingFrequency(20000)
  # enableProfiling()
  let startTime = getMonoTime()
  for i in 0..<1000:
    if i mod 100 == 0:
      echo i
    let t = f.fitParams(vars, y)
  echo getMonoTime() - startTime
  #echo t
  
  #var
  #  jacobian = newTensorUninit[Number](5, 2)
  #  linearParams = newTensorUninit[Number](3)
  #f.fillData(vars, startParams, y, n, varCount, p, pl, jacobian, linearParams)
import random, math
import matrix
from sequtils import repeat

func randomValue(r: var Rand): Number =
  const H = ln(50.0).Number
  const L = -H
  result = exp(r.rand(L..H)) * r.sample([1.0, -1.0])


proc randomParams(r: var Rand, n: int): Vector =
  result = vector(n)
  for i in 0..<n:
    result[i] = r.randomValue()

iterator generateInitialParams*(n: int): Vector =
  yield repeat(Number(0), n).vector
  yield repeat(Number(1), n).vector
  var r = initRand(1234)
  for i in 0..<30:
    yield r.randomParams(n)
import random, math
from expressions import Number
import arraymancer
from sequtils import repeat

func randomValue(r: var Rand): Number =
  const H = ln(1500.0).Number
  const L = -H
  result = exp(r.rand(L..H)) * r.sample([1.0, -1.0])


proc randomParams(r: var Rand, n: int): seq[Number] =
  result = newSeq[Number](n)
  for i in 0..<n:
    result[i] = r.randomValue()

iterator generateInitialParams*(n: int): seq[Number] =
  yield repeat(Number(0), n)
  yield repeat(Number(1), n)
  var r = initRand(1234)
  for i in 0..<30:
    yield r.randomParams(n)
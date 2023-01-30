type DataObj* = object
  trainX*, approxX*: seq[seq[float64]]
  trainY*, approxY*: seq[float64]
  errorBound*: float64
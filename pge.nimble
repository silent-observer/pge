# Package

version       = "0.1.0"
author        = "silent-observer"
description   = "Prioritized Grammar Evaluation curve fitter (symbolic regression)"
license       = "MIT"
srcDir        = "src"
binDir        = "bin"
bin           = @["pge", "network/evalserver"]


# Dependencies

requires "nim >= 1.6.2"
requires "nimlapack"
requires "nimblas"
requires "lrucache"

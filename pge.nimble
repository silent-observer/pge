# Package
import os

version       = "0.1.0"
author        = "silent-observer"
description   = "Prioritized Grammar Evaluation curve fitter (symbolic regression)"
license       = "MIT"
srcDir        = "src"
binDir        = "bin"
bin           = @["pge", "network/evalserver", "web/webserver"]


# Dependencies

requires "nim >= 1.6.2"
requires "nimlapack"
requires "nimblas"
requires "lrucache"
requires "nimja"
requires "cpuwhat"

task buildJS, "Builds the JS side":
  requires "ajax"
  for f in walkDir("src" / "web" / "js", relative=true):
    if f.kind in {pcDir, pcLinkToDir}:
      mkDir "bin" / "web" / "static" / "js" / f.path
    else:
      exec "nim js -o:" & 
        changeFileExt("bin" / "web" / "static" / "js" / f.path, "js") & 
        " " & ("src" / "web" / "js" / f.path)
    
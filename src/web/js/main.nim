import dom, jsconsole
import strutils, sugar, json
import ../types
import ajax

window.onload = proc(_: Event) =
  document.getElementById("submitbtn").addEventListener("click", proc(_: Event) =
    let xdataStr = $document.getElementById("xdata").value
    let ydataStr = $document.getElementById("ydata").value
    let xdata = collect:
      for l in xdataStr.splitLines:
        collect:
          for n in l.split({' ', '\t'}):
            parseFloat($n).float64
    let ydata = collect:
      for n in ydataStr.split({' ', '\t'}):
        parseFloat($n).float64
    let n = xdata.len
    const PeekCoefficient = 0.1
    let peekN = int(PeekCoefficient * n.float)

    let xapproxdata = xdata[0..<peekN]
    let yapproxdata = ydata[0..<peekN]
    let dataobj = DataObj(
      trainX: xdata,
      trainY: ydata,
      approxX: xapproxdata,
      approxY: yapproxdata,
      errorBound: 1e-7
    )

    var httpRequest = newXMLHttpRequest()
    httpRequest.onreadystatechange = proc(e: Event) =
      if httpRequest.readyState == rsDONE:
        if httpRequest.status == 200:
          console.log(httpRequest.responseText)
          window.location.href = cstring("/progress?stream=" & $httpRequest.responseText)
        else:
          console.log("Error!")
    httpRequest.open("POST", "/senddata")
    httpRequest.send(cstring($(%*dataobj)))
  )
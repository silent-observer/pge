import dom, jsconsole, jsffi, jsre
import strutils, sugar, json
import strformat
import ../types
import ajax

var cometIndex = 0
var streamId {.importc, nodecl.}: cstring
var messagesToDisplay: seq[cstring]

func replaceAll*(pattern: cstring; self: RegExp; replacement: cstring): cstring {.importjs: "#.replaceAll(#, #)".}

proc redrawMessages() =
  let messageSection = getElementById("messages")
  messageSection.innerHTML = ""

  for m in messagesToDisplay:
    var obj = document.createElement("div")
    obj.innerText = m
    messageSection.appendChild obj
  {.emit: "window.MathJax.typeset();".}

proc transformMessage(m: cstring): cstring =
  template r(m: cstring, regex: string, replacement: string): cstring =
    m.replaceAll(newRegExp(regex, r"g"), replacement)
  
  result = m
    .r(r"(\d+:\s*)([^\n]+)\n", "$1\\[$2\\]\n")
    .r(r"([axc])(\d+)", r"$1_{$2}")
    .r(r" \* ", r" ")
    .r(r"([^+\[\(]*)1\/\(((?:[^()]|\((?:[^()]|\([^()]+\)|)+\)|)+)\)", r"\frac{$1}{$2}")
    .r(r"(exp|a?sin|a?cos|sqrt|erf)", r"\$1")
    .r(r"atan2", r"\text{atan2}")
    .r(r"\(", r"\left(")
    .r(r"\)", r"\right)")

proc comet() =
  var httpRequest = newXMLHttpRequest()
  httpRequest.onreadystatechange = proc(e: Event) =
    if httpRequest.readyState == rsDONE:
      if httpRequest.status == 200:
        let obj = parseJson($httpRequest.responseText)
        var messages = newSeqOfCap[cstring](obj["messages"].len)
        for m in obj["messages"]:
          messages.add m.getStr.cstring.transformMessage()
        if messages.len > 0:
          let newCometIndex = obj["index"].getInt(0)
          if newCometIndex > messagesToDisplay.len:
            messagesToDisplay.setLen(newCometIndex)
          messagesToDisplay[cometIndex..^1] = messages
          cometIndex = newCometIndex
          redrawMessages()
          discard setTimeout(comet, 0)
      else:
        console.log("Error!")
  httpRequest.open("GET", cstring(fmt"/comet?stream={streamId}&index={cometIndex}"))
  httpRequest.send()

window.onload = proc(_: Event) =
  comet()
import dom, jsconsole, jsffi
import strutils, sugar, json
import strformat
import ../types
import ajax

var cometIndex = 0
var streamId {.importc, nodecl.}: cstring
var messagesToDisplay: seq[string]

proc redrawMessages() =
  let messageSection = getElementById("messages")
  messageSection.innerHTML = ""

  for m in messagesToDisplay:
    var obj = document.createElement("p")
    obj.innerText = cstring(m)
    messageSection.appendChild obj

proc comet() =
  var httpRequest = newXMLHttpRequest()
  httpRequest.onreadystatechange = proc(e: Event) =
    if httpRequest.readyState == rsDONE:
      if httpRequest.status == 200:
        let obj = parseJson($httpRequest.responseText)
        var messages = newSeqOfCap[string](obj["messages"].len)
        for m in obj["messages"]:
          messages.add m.getStr
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
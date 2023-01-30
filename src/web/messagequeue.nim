import std/asyncdispatch

type
  Message* = string
  Callback = proc() {.gcsafe.}
  MessageQueue* = ref object
    messages: seq[Message]
    callbacks: seq[Callback]

proc add*(q: MessageQueue, m: Message) =
  q.messages.add m
  for c in q.callbacks:
    c()
  q.callbacks.setLen 0

proc getSince*(q: MessageQueue, index: int): Future[(seq[Message], int)] {.async.} =
  if q.messages.high <= index:
    var waitFuture = newFuture[void]("getSince")
    q.callbacks.add(proc() = waitFuture.complete())
    await waitFuture
  
  return (q.messages[index+1..^1], q.messages.high)
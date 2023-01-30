import std/asyncdispatch
import messagequeue
import tables
from random import sample
from strutils import join
from sugar import collect
export Message

type
  MessageStreamID* = string
  MessageDB* = object
    loaded*: Table[MessageStreamID, MessageQueue]

proc generateMessageStreamID*(): MessageStreamID =
  let list = collect:
    for _ in 0..<10:
      $sample({'0'..'9', 'a'..'z'})
  list.join("")

proc initMessageDB*(): MessageDB {.inline.} =
  result.loaded = initTable[MessageStreamID, MessageQueue]()

proc load(q: var MessageDB, stream: MessageStreamID) {.inline.} =
  if stream notin q.loaded:
    q.loaded[stream] = MessageQueue()

proc add*(q: var MessageDB, stream: MessageStreamID, m: Message) {.inline.} =
  q.load(stream)
  q.loaded[stream].add m

proc getSince*(q: var MessageDB, stream: MessageStreamID, index: int): Future[(seq[Message], int)] {.inline.} =
  q.load(stream)
  q.loaded[stream].getSince index
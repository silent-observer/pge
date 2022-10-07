type
  TrieNode[N: static int] {.acyclic.} = ref object
    children: array[N, TrieNode[N]]
    hasValue: bool
  Trie*[N: static int] = distinct TrieNode[N]

func newTrie*[N: static int](): Trie[N] {.inline.} =
  result = new(TrieNode[N]).Trie[:N]

func add*[N: static int](t: Trie[N], data: seq[int]) =
  var n = TrieNode[N](t)
  for x in data:
    if n.children[x] == nil:
      n.children[x] = TrieNode[N]()
    n = n.children[x]
  n.hasValue = true

func contains*[N: static int](t: Trie[N], data: seq[int]): bool =
  var n = TrieNode[N](t)
  for x in data:
    if n.children[x] == nil:
      return false
    n = n.children[x]
  result = n.hasValue
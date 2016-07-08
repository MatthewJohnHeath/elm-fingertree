module Node exposing(..)
{-  (Node,Node(Node2), Node(Node3),
  map, reduce,
  toFinger,
  split)-}
import Finger

type Node a
  = Node2 a a
  | Node3 a a a

map: (a -> b) -> Node a -> Node b
map f node =
  case node of
    Node2 a1 a2 -> Node2 (f a1) (f a2)
    Node3 a1 a2 a3 -> Node3 (f a1) (f a2) (f a3)

reduce: (a -> b -> b) -> b -> Node a -> b
reduce f zero node =
  case node of
    Node2 a1 a2 -> zero |> f a1 |> f a2
    Node3 a1 a2 a3 -> zero |> f a1 |> f a2 |> f a3

toFinger: Node a -> Finger.Finger a
toFinger node =
  case node of
    Node2 a1 a2 -> Finger.Two a1 a2
    Node3 a1 a2 a3 -> Finger.Three a1 a2 a3

startSplit: Node a-> Finger.ListSplit a
startSplit node =
  case node of
    Node2 a1 a2 -> {left= [], mid = a1, right =[a2]}
    Node3 a1 a2 a3 -> {left= [], mid = a1, right =[a2, a3]}

split: b -> (a->b->b) -> (b-> Bool) -> Node a ->  Finger.ListSplit a
split start folder measurement node =
  let loop acc split =
    let newacc = folder split.mid  acc in
    if measurement newacc  then split
    else case split.right of
      [] -> split
      a::tail ->
          loop newacc {left=List.append split.left [split.mid], mid =a, right =tail  }
    in
    loop start (startSplit node)

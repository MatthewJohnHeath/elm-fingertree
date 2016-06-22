module FingerTree exposing(..)
import Finger exposing(Finger, Finger(One))
import Node exposing(Node, Node(..))
import Maybe

type FingerTree a
  =Empty
  |Single a
  |Deep (Finger a) (FingerTree(Node a)) (Finger a)

ofFinger: Finger.Finger a -> FingerTree a
ofFinger finger =
  case finger of
    Finger.One a1 -> Single a1
    Finger.Two a1 a2 -> Deep (Finger.One a1) Empty (Finger.One a2)
    Finger.Three a1 a2 a3 -> Deep (Finger.Two a1 a2) Empty (Finger.One a3)
    Finger.Four a1 a2 a3 a4 -> Deep (Finger.Two a1 a2) Empty (Finger.Two a3 a4)

leftPush: a -> FingerTree a -> FingerTree a
leftPush a0 fingerTree=
  case fingerTree of
    Empty -> Single a0
    Single a1 -> Deep (Finger.One a0) Empty (Finger.One a1)
    Deep (Finger.Four a1 a2 a3 a4) middle right ->
      Deep (Finger.Two a0 a1) (leftPush(Node.Node3 a2 a3 a4) middle) right
    Deep left middle right ->  Deep (Finger.leftAdd a0 left) middle right

leftPeak: FingerTree a -> Maybe.Maybe a
leftPeak tree =
  case tree of
    Empty -> Nothing
    Single a -> Just a
    Deep left _ _ -> Just (Finger.leftPeak left)

leftPop: FingerTree a -> FingerTree a
leftPop tree =
  case tree of
    Empty -> Empty
    Single _ -> Empty
    Deep (Finger.One a ) mid right ->
      case leftPeak mid of
      Just node -> Deep (Node.toFinger node) (leftPop mid) right
      Nothing -> ofFinger right
    Deep left mid right -> Deep (Finger.leftRemove left) mid right

rightPush: a -> FingerTree a -> FingerTree a
rightPush a0 fingerTree=
  case fingerTree of
    Empty -> Single a0
    Single a1 -> Deep (Finger.One a1) Empty (Finger.One a0)
    Deep left middle (Finger.Four a4 a3 a2 a1)->
      Deep left (rightPush(Node.Node3 a4 a3 a2) middle) (Finger.Two a1 a0)
    Deep left middle right ->  Deep left middle (Finger.rightAdd a0 right)

rightPeak: FingerTree a -> Maybe.Maybe a
rightPeak tree =
  case tree of
    Empty -> Nothing
    Single a -> Just a
    Deep _ _ right -> Just (Finger.rightPeak right)

rightPop: FingerTree a -> FingerTree a
rightPop tree =
  case tree of
    Empty -> Empty
    Single _ -> Empty
    Deep  left mid (Finger.One a ) ->
      case rightPeak mid of
      Just node -> Deep left (rightPop mid) (Node.toFinger node)
      Nothing -> ofFinger left
    Deep left mid right -> Deep left mid (Finger.rightRemove right)

map : (a -> b) -> FingerTree a -> FingerTree b
map f tree =
  case tree of
    Empty -> Empty
    Single a0 -> Single (f a0)
    Deep left mid right ->
      Deep (Finger.map f left) (map (Node.map f) mid ) (Finger.map f right)

reduce: (a-> b -> b) -> b -> FingerTree a -> b
reduce f zero tree=
    case tree of
      Empty -> zero
      Single a0 -> f a0 zero
      Deep left mid right ->
        let leftReduced = Finger.reduce f zero left in
        let midReduced = reduce (flip(Node.reduce f)) leftReduced mid in
        Finger.reduce f midReduced right

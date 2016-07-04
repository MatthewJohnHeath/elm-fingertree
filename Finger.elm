module Finger exposing(..)
  {-(Finger(..),
  map, reduce,
  leftAdd, rightAdd,
  leftRemove, rightRemove,
  leftPeak, rightPeak,
  tryFromList, toList,
  ListSplit, split
 -}

import Monoid exposing (Monoid)
 {- |A container of 1 to 4 elements -}
type Finger a
  = One a
  | Two a a
  | Three a a a
  | Four a a a a

map: (a -> b) -> Finger a -> Finger b
map f finger =
  case finger of
    One a1 -> One (f a1)
    Two a1 a2 -> Two (f a1) (f a2)
    Three a1 a2 a3 -> Three (f a1) (f a2) (f a3)
    Four  a1 a2 a3 a4 -> Four (f a1) (f a2) (f a3) (f a4)

reduce: (a -> b -> b) -> b -> Finger a -> b
reduce f zero finger =
  case finger of
    One a1 -> zero |> f a1
    Two a1 a2 -> zero |> f a1 |> f a2
    Three a1 a2 a3 -> zero |> f a1 |> f a2 |> f a3
    Four  a1 a2 a3 a4 -> zero |> f a1 |> f a2 |> f a3 |> f a4

leftAdd: a -> Finger a -> Finger a
leftAdd ana finger =
  case finger of
    One a1 -> Two ana a1
    Two a1 a2 -> Three ana a1 a2
    Three a1 a2 a3 -> Four ana a1 a2 a3
    Four  a1 a2 a3 a4 -> finger

rightAdd: a -> Finger a -> Finger a
rightAdd ana finger =
  case finger of
    One a1 -> Two a1 ana
    Two a1 a2 -> Three a1 a2 ana
    Three a1 a2 a3 ->
       Four a1 a2 a3 ana
    Four  a1 a2 a3 a4 -> finger

leftRemove: Finger a -> Finger a
leftRemove finger=
  case finger of
    One a1 -> finger
    Two a1 a2 -> One a2
    Three a1 a2 a3 -> Two a2 a3
    Four  a1 a2 a3 a4 -> Three a2 a3 a4

rightRemove: Finger a -> Finger a
rightRemove finger=
  case finger of
    One a1 -> finger
    Two a1 a2 -> One a1
    Three a1 a2 a3 -> Two a1 a2
    Four  a1 a2 a3 a4 -> Three a1 a2 a3

leftPeak:Finger a->a
leftPeak finger =
  case finger of
  One a'->a'
  Two a' _ ->a'
  Three a' _ _-> a'
  Four a' _ _ _ -> a'

rightPeak:Finger a -> a
rightPeak finger =
    case finger of
    One a'->a'
    Two _ a'  ->a'
    Three  _ _ a'-> a'
    Four _ _ _  a'-> a'

tryFromList: List a -> Maybe (Finger a)
tryFromList list =
  case list of
    [a1]-> Just (One a1)
    a1::a2::[] -> Just (Two a1 a2)
    a1::a2::a3::[]-> Just (Three a1 a2 a3)
    a1::a2::a3::a4::[]-> Just (Four a1 a2 a3 a4)
    _-> Nothing

toList:Finger a -> List a
toList finger=
  case finger of
    One a1 -> [a1]
    Two a1 a2 -> [a1, a2]
    Three a1 a2 a3 -> [a1, a2, a3]
    Four  a1 a2 a3 a4 -> [a1, a2, a3, a4]

type alias ListSplit a=
  {left: List a, mid: a, right: List a }

startSplit: Finger a -> ListSplit a
startSplit finger =
  case finger of
    One a ->{left =[], mid = a, right =[] }
    _ ->
      {left = [], mid = leftPeak finger, right = finger |> leftRemove |> toList}

splitList:  (a -> b -> b) -> (b -> Bool) -> b ->ListSplit a ->  Maybe (ListSplit a)
splitList folder measurement accumulator listsplit =
  let newacc = folder listsplit.mid accumulator in
  if  measurement newacc then Just listsplit
  else
    case listsplit.right of
      [] -> Nothing
      newmid::newright ->
        splitList folder measurement newacc {left =  listsplit.left ++ [listsplit.mid], mid = newmid, right = newright}

split: (a -> b -> b) -> (b -> Bool) -> b -> Finger a ->  Maybe (ListSplit a)
split folder measurement accumulator  =startSplit >> (splitList folder measurement accumulator)

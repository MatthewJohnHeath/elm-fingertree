module Annotated exposing (..)
import Internal exposing (InternalFingerTree)
import Monoid exposing(Monoid)

type alias AnnotatedFingerTree a b
  ={
  monoid : Monoid b ,
  projection: a -> b,
  tree : InternalFingerTree a b}

empty:Monoid b  -> (a -> b) -> AnnotatedFingerTree a b
empty monoid  projection = AnnotatedFingerTree monoid projection Internal.empty

reduce: AnnotatedFingerTree a b -> b
reduce aft = Internal.reduce aft.monoid aft.projection aft.tree

leftPeek: AnnotatedFingerTree a b -> Maybe a
leftPeek = .tree>>Internal.leftPeak

leftPop: AnnotatedFingerTree a b -> AnnotatedFingerTree a b
leftPop aft =
  {aft| tree = Internal.leftPop  aft.monoid aft.tree}

leftPush: a -> AnnotatedFingerTree a b -> AnnotatedFingerTree a b
leftPush ana aft =
  {aft| tree = Internal.leftPush  aft.monoid aft.projection ana  aft.tree}

rightPeek: AnnotatedFingerTree a b -> Maybe a
rightPeek = .tree>>Internal.rightPeak

rightPop: AnnotatedFingerTree a b -> AnnotatedFingerTree a b
rightPop aft =
  {aft| tree = Internal.rightPop  aft.monoid aft.tree}

rightPush: a -> AnnotatedFingerTree a b -> AnnotatedFingerTree a b
rightPush ana aft =
  {aft| tree = Internal.rightPush  aft.monoid aft.projection ana  aft.tree}

concat: AnnotatedFingerTree a b -> AnnotatedFingerTree a b ->AnnotatedFingerTree a b
concat l r = {l|tree=Internal.concat l.monoid l.projection l.tree r.tree}

concatWithMiddle: AnnotatedFingerTree a b -> List a -> AnnotatedFingerTree a b ->AnnotatedFingerTree a b
concatWithMiddle l m r ={l|tree=Internal.concatWithMiddle l.monoid l.projection l.tree m r.tree}
--splitting
type alias Split a b =
  {left: AnnotatedFingerTree a b,
  middle:a,
  right: AnnotatedFingerTree a b }

annotate: AnnotatedFingerTree a b -> Internal.Split a b -> Split a b
annotate aft asplit =
  {left = {aft|tree=asplit.left}, middle = asplit.middle, right = {aft|tree=asplit.right}}

split: (b -> Bool) -> AnnotatedFingerTree a b -> Maybe(Split a b)
split measure aft=
     aft.tree
     |> Internal.split aft.monoid aft.projection measure aft.monoid.zero
     |> Maybe.map(annotate aft)


get : (b -> Bool)-> AnnotatedFingerTree a b -> Maybe a
get measure = split measure >>(Maybe.map .middle)

remove : (b -> Bool) -> AnnotatedFingerTree a b -> Maybe (AnnotatedFingerTree a b)
remove measure  =
 (split measure) >> (Maybe.map (\asplit -> concat asplit.left asplit.right))

set : (b -> Bool) -> a -> AnnotatedFingerTree a b -> Maybe (AnnotatedFingerTree a b)
set measure value =
  let rebuild asplit=
    concatWithMiddle asplit.left [value] asplit.right
  in
  split measure >> Maybe.map(rebuild)

insert : (b -> Bool) -> a -> AnnotatedFingerTree a b -> Maybe (AnnotatedFingerTree a b)
insert measure value =
  let rebuild asplit=
    concatWithMiddle asplit.left [value, asplit.middle] asplit.right
  in
  split measure >> Maybe.map (rebuild)

--slices--
before: (b -> Bool) -> AnnotatedFingerTree a b -> AnnotatedFingerTree a b
before measure aft =
  case split measure aft of
    Just asplit-> asplit.left
    Nothing -> {aft| tree=Internal.Empty}

upTo : (b -> Bool) -> AnnotatedFingerTree a b -> AnnotatedFingerTree a b
upTo measure aft =
  case split measure aft of
    Just asplit-> asplit.left |> rightPush asplit.middle
    Nothing -> {aft| tree=Internal.Empty}

after: (b -> Bool) -> AnnotatedFingerTree a b -> AnnotatedFingerTree a b
after measure aft =
  case split measure aft of
    Just asplit-> asplit.right
    Nothing ->  {aft| tree=Internal.Empty}

from : (b -> Bool) -> AnnotatedFingerTree a b -> AnnotatedFingerTree a b
from measure aft =
  case split measure aft of
    Just asplit-> asplit.right |> leftPush asplit.middle
    Nothing ->  {aft| tree=Internal.Empty}

module Annotated exposing (..)
{-|Annotated finger tree
# Definitions
@docs AnnotatedFingerTree, Split

# Create
@docs empty, reduce

# Dequeue operations
@docs leftPeek, leftPop,leftPush, rightPeek, rightPop, rightPush

# Concatanation
@docs concat, concatWithMiddle

#Splits
@docs annotate, split

#Array operations
@docs get, set, remove, insert

#Slices
@docs before, after, upTo, from
-}
import Internal exposing (InternalFingerTree)
import Monoid exposing(Monoid)
{-|Finger tree annotated by a Monoid-}
type alias AnnotatedFingerTree a b
  ={
  monoid : Monoid b ,
  projection: a -> b,
  tree : InternalFingerTree a b}
{-| Empty finger tree specifiying monoid and projection-}
empty:Monoid b  -> (a -> b) -> AnnotatedFingerTree a b
empty monoid projection = AnnotatedFingerTree monoid projection Internal.empty
{-|reduce-}
reduce: AnnotatedFingerTree a b -> b
reduce aft = Internal.reduce aft.monoid aft.projection aft.tree
{-|returns the leftmost element-}
leftPeek: AnnotatedFingerTree a b -> Maybe a
leftPeek = .tree>>Internal.leftPeak
{-|removes from left-}
leftPop: AnnotatedFingerTree a b -> AnnotatedFingerTree a b
leftPop aft =
  {aft| tree = Internal.leftPop  aft.monoid aft.tree}
{-|adds to left-}
leftPush: a -> AnnotatedFingerTree a b -> AnnotatedFingerTree a b
leftPush ana aft =
  {aft| tree = Internal.leftPush  aft.monoid aft.projection ana  aft.tree}
{-|returns rightmost element-}
rightPeek: AnnotatedFingerTree a b -> Maybe a
rightPeek = .tree>>Internal.rightPeak
{-| remove from right-}
rightPop: AnnotatedFingerTree a b -> AnnotatedFingerTree a b
rightPop aft =
  {aft| tree = Internal.rightPop  aft.monoid aft.tree}
{-|add to right-}
rightPush: a -> AnnotatedFingerTree a b -> AnnotatedFingerTree a b
rightPush ana aft =
  {aft| tree = Internal.rightPush  aft.monoid aft.projection ana  aft.tree}
{-| concatanate-}
concat: AnnotatedFingerTree a b -> AnnotatedFingerTree a b ->AnnotatedFingerTree a b
concat l r = {l|tree=Internal.concat l.monoid l.projection l.tree r.tree}
{-| concatanate trees left and right with a list in between-}
concatWithMiddle: AnnotatedFingerTree a b -> List a -> AnnotatedFingerTree a b ->AnnotatedFingerTree a b
concatWithMiddle l m r ={l|tree=Internal.concatWithMiddle l.monoid l.projection l.tree m r.tree}
--splitting
{-|Like a zipper-}
type alias Split a b =
  {left: AnnotatedFingerTree a b,
  middle:a,
  right: AnnotatedFingerTree a b }
{-|annotate-}
annotate: AnnotatedFingerTree a b -> Internal.Split a b -> Split a b
annotate aft asplit =
  {left = {aft|tree=asplit.left}, middle = asplit.middle, right = {aft|tree=asplit.right}}
{-|Let & be the monoid operation
  split measure aft returns the split at the leftmost "middle" such that ( (reduce split left ) & split.middle ) |> measure = true-}
split: (b -> Bool) -> AnnotatedFingerTree a b -> Maybe(Split a b)
split measure aft=
     aft.tree
     |> Internal.split aft.monoid aft.projection measure aft.monoid.zero
     |> Maybe.map(annotate aft)

{-|get element when measure turns true-}
get : (b -> Bool)-> AnnotatedFingerTree a b -> Maybe a
get measure = split measure >>(Maybe.map .middle)
{-|remove element when measure turns true-}
remove : (b -> Bool) -> AnnotatedFingerTree a b -> Maybe (AnnotatedFingerTree a b)
remove measure  =
 (split measure) >> (Maybe.map (\asplit -> concat asplit.left asplit.right))
{-|set element when measure turns true-}
set : (b -> Bool) -> a -> AnnotatedFingerTree a b -> Maybe (AnnotatedFingerTree a b)
set measure value =
  let rebuild asplit=
    concatWithMiddle asplit.left [value] asplit.right
  in
  split measure >> Maybe.map(rebuild)
{-|insert element before measure turns true-}
insert : (b -> Bool) -> a -> AnnotatedFingerTree a b -> Maybe (AnnotatedFingerTree a b)
insert measure value =
  let rebuild asplit=
    concatWithMiddle asplit.left [value, asplit.middle] asplit.right
  in
  split measure >> Maybe.map (rebuild)

--slices--
{-|elements before measure turns true-}
before: (b -> Bool) -> AnnotatedFingerTree a b -> AnnotatedFingerTree a b
before measure aft =
  case split measure aft of
    Just asplit-> asplit.left
    Nothing -> {aft| tree=Internal.Empty}
{-|elements before and equal to when measure turns true-}
upTo : (b -> Bool) -> AnnotatedFingerTree a b -> AnnotatedFingerTree a b
upTo measure aft =
  case split measure aft of
    Just asplit-> asplit.left |> rightPush asplit.middle
    Nothing -> {aft| tree=Internal.Empty}
{-|elements after measure turns true-}
after: (b -> Bool) -> AnnotatedFingerTree a b -> AnnotatedFingerTree a b
after measure aft =
  case split measure aft of
    Just asplit-> asplit.right
    Nothing ->  {aft| tree=Internal.Empty}
{-|elements after and equal to when measure turns true-}
from : (b -> Bool) -> AnnotatedFingerTree a b -> AnnotatedFingerTree a b
from measure aft =
  case split measure aft of
    Just asplit-> asplit.right |> leftPush asplit.middle
    Nothing ->  {aft| tree=Internal.Empty}

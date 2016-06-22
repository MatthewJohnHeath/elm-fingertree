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

leftPeak: AnnotatedFingerTree a b -> Maybe a
leftPeak = .tree>>Internal.leftPeak

leftPop: AnnotatedFingerTree a b -> AnnotatedFingerTree a b
leftPop aft =
  {aft| tree = Internal.leftPop  aft.monoid aft.tree}

leftPush: a -> AnnotatedFingerTree a b -> AnnotatedFingerTree a b
leftPush ana aft =
  {aft| tree = Internal.leftPush  aft.monoid aft.projection ana  aft.tree}

rightPeak: AnnotatedFingerTree a b -> Maybe a
rightPeak = .tree>>Internal.rightPeak

rightPop: AnnotatedFingerTree a b -> AnnotatedFingerTree a b
rightPop aft =
  {aft| tree = Internal.rightPop  aft.monoid aft.tree}

rightPush: a -> AnnotatedFingerTree a b -> AnnotatedFingerTree a b
rightPush ana aft =
  {aft| tree = Internal.rightPush  aft.monoid aft.projection ana  aft.tree}

concat: AnnotatedFingerTree a b -> AnnotatedFingerTree a b ->AnnotatedFingerTree a b
concat l r = {l|tree=Internal.concat l.monoid l.projection l.tree r.tree} 

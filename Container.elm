module Container exposing(..)
import Annotated exposing(AnnotatedFingerTree)
import Monoid
type alias Container a = AnnotatedFingerTree a Int

empty : Container a
empty = Annotated.empty Monoid.int  (always 1)

fromList: List a -> Container a
fromList = List.foldl pushBack empty

{-Pushes and peaks and pops constant time
-}
pushFront: a -> Container a -> Container a
pushFront = Annotated.leftPush

popFront: Container a -> Container a
popFront = Annotated.leftPop

peekFront: Container a -> Maybe a
peekFront = Annotated.leftPeek

pushBack: a -> Container a -> Container a
pushBack = Annotated.rightPush

popBack: Container a -> Container a
popBack = Annotated.rightPop

peekBack: Container a -> Maybe a
peekBack = Annotated.rightPeek

size: Container a -> Int
size = Annotated.reduce
{-random access-}
get: Int -> Container a -> Maybe a
get  = Annotated.get << (<)

set: Int -> a ->  Container a -> Maybe(Container a)
set = Annotated.set << (<)

remove: Int -> Container a -> Maybe(Container a)
remove = Annotated.remove << (<)

insert : Int -> a ->  Container a -> Maybe(Container a)
insert = Annotated.insert << (<)

slice: Int -> Int -> Container a -> Container a
slice start end con=
  let length = size con in
  let index n=
    if (n < -length) then index 0
    else
    if (n < 0) then index (length + n)
    else
    if (n >= length) then length - 1
    else n
  in
  con
  |> Annotated.from (index >> ((<)start) )
  |> Annotated.before (index >> ((<)(end-start)) )

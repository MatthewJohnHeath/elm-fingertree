module Container exposing(..)
{-| Dequeue with random access
# Definition
@docs Container

# Create
@docs empty, fromList

# Dequeue Operations
@docs pushFront, popFront, peekFront, pushBack, popBack, peekBack, size

# Random Access
@docs get, set, remove, insert, slice
-}
import Annotated exposing(AnnotatedFingerTree)
import Monoid
{-|A container allowing constant time dequeue operations and log time access -}
type alias Container a = AnnotatedFingerTree a Int
{-|An empty container-}
empty : Container a
empty = Annotated.empty Monoid.int  (always 1)
{-|Make a container from a list-}
fromList: List a -> Container a
fromList = List.foldl pushBack empty

{-| Add to the front-}
pushFront: a -> Container a -> Container a
pushFront = Annotated.leftPush
{-|Take from the front-}
popFront: Container a -> Container a
popFront = Annotated.leftPop
{-|See the front element-}
peekFront: Container a -> Maybe a
peekFront = Annotated.leftPeek
{-|Add at the back-}
pushBack: a -> Container a -> Container a
pushBack = Annotated.rightPush
{-|Take from the back-}
popBack: Container a -> Container a
popBack = Annotated.rightPop
{-|See the back element-}
peekBack: Container a -> Maybe a
peekBack = Annotated.rightPeek
{-|The number of elements in the collection-}
size: Container a -> Int
size = Annotated.reduce
{-| Get the element at an index-}
get: Int -> Container a -> Maybe a
get  = Annotated.get << (<)
{-|Set the element at an index-}
set: Int -> a ->  Container a -> Maybe(Container a)
set = Annotated.set << (<)
{-|Remove the element at an index mobing later elements to the left-}
remove: Int -> Container a -> Maybe(Container a)
remove = Annotated.remove << (<)
{-|Insert an element at an index moving later elements to the right-}
insert : Int -> a ->  Container a -> Maybe(Container a)
insert = Annotated.insert << (<)
{-|Get a sub-section of an array: (slice start end array). The start is a zero-based index where we will start our slice.
The end is a zero-based index that indicates the end of the slice. The slice extracts up to but not including end.-}
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

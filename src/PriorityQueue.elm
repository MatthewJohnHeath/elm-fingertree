module PriorityQueue exposing(PriorityQueue, empty, topPriority, top, pop, push)
{-|Purely functional priority queue
#Definition
@docs PriorityQueue
#Create
@docs empty
# Priority queue operations
@docs topPriority, top, pop, push
-}
import Annotated exposing(AnnotatedFingerTree)
import Monoid
{-|Persistant prirotiy queue. Constant time push and access to top. -}
type alias PriorityQueue a comparable=
  AnnotatedFingerTree (Monoid.WithAppendedZero (a,comparable)) (Monoid.WithAppendedZero (a,comparable))

minBySnd: (a,comparable) -> (a,comparable) -> (a,comparable)
minBySnd (a,c) (a_,c_)= if c_>c then (a_,c_) else (a,c)
{-| returns an empty priority queue-}
empty: PriorityQueue a comparable
empty = Annotated.empty (Monoid.appendZero minBySnd) identity
{-|Returns the highest priority in the priority queue-}
topPriority : (PriorityQueue a comparable) -> Maybe comparable
topPriority pq=
  case Annotated.reduce pq of
    Monoid.Zero -> Nothing
    Monoid.Normal (value, priority) -> Just priority
{-|Returns the value with the highest prority-}
top : (PriorityQueue a comparable) -> Maybe a
top pq =
  case Annotated.reduce pq of
    Monoid.Zero -> Nothing
    Monoid.Normal (value, priority) -> Just value
{-|Remove the element with the top priority. Returns Nothing if input was empty-}
pop : (PriorityQueue a comparable) -> Maybe (PriorityQueue a comparable)
pop pq =
  let measure waz =
    case (waz, topPriority pq) of
      ( Monoid.Normal (value, priority), Just topP) ->
        priority >= topP
      _ -> False
  in
  Annotated.remove measure pq
{-|Add element to priority queue-}
push : comparable -> a -> PriorityQueue a comparable -> PriorityQueue a comparable
push priority value =
   (value, priority) |>Monoid.Normal |> Annotated.leftPush

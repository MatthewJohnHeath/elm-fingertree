module PriorityQueue exposing(..)
import Annotated exposing(AnnotatedFingerTree)
import Monoid
type alias PriorityQueue a comparable=
  AnnotatedFingerTree (Monoid.WithAppendedZero (a,comparable)) (Monoid.WithAppendedZero (a,comparable))

minBySnd: (a,comparable) -> (a,comparable) -> (a,comparable)
minBySnd (a,c) (a',c')= if c'>c then (a',c') else (a,c)

empty: PriorityQueue a comparable
empty = Annotated.empty (Monoid.appendZero minBySnd) identity

topPriority : (PriorityQueue a comparable) -> Maybe comparable
topPriority pq=
  case Annotated.reduce pq of
    Monoid.Zero -> Nothing
    Monoid.Normal (value, priority) -> Just priority
top : (PriorityQueue a comparable) -> Maybe a
top pq =
  case Annotated.reduce pq of
    Monoid.Zero -> Nothing
    Monoid.Normal (value, priority) -> Just value

pop : (PriorityQueue a comparable) -> Maybe (PriorityQueue a comparable)
pop pq =
  let measure waz =
    case (waz, topPriority pq) of
      ( Monoid.Normal (value, priority), Just topP) ->
        priority >= topP
      _ -> False
  in
  Annotated.remove measure pq

push : comparable -> a -> PriorityQueue a comparable -> PriorityQueue a comparable
push priority value =
   (value, priority) |>Monoid.Normal |> Annotated.leftPush 

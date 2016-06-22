module HackyFingerTree exposing(..)

type FingerTree a
  =Empty
  |Single a
  |Deep (List a) (FingerTree(List a)) (List a)

leftAdd: a -> FingerTree a -> FingerTree a
leftAdd a0 fingerTree=
  case fingerTree of
    Empty -> Single a0
    Single a1 -> Deep  [a0] Empty [a1]
    Deep left middle right ->
      case left of
        [a1, a2, a3, a4] ->
          Deep [a0, a1] ( leftAdd [a2, a3, a4] middle) right
        _ -> Deep (a0::left) middle right

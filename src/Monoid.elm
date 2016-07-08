module Monoid exposing(
  Monoid,  int, float,
  WithAppendedZero, WithAppendedZero(..), appendZero)

type alias Monoid a = {zero: a , op : a -> a -> a }

apply: Monoid a -> a -> a -> a
apply mon a1 a2 = mon.op a1 a2

type WithAppendedZero a
  = Zero
  |Normal a

appendZero: (a-> a -> a) -> Monoid (WithAppendedZero a)
appendZero op =
  let newOp a1 a2 =
    case (a1,a2) of
    (Zero,  a) -> a
    (a, Zero) -> a
    (Normal a, Normal a') -> Normal(op a a')
  in
  {zero = Zero , op = newOp}


int: Monoid Int
int = {zero = 0, op = (+)}

float : Monoid Float
float = {zero = 0.0, op =(+)}

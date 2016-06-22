module Monoid exposing(Monoid, maximum, int, float)

type alias Monoid a = {zero: a , op : a -> a -> a }

apply: Monoid a -> a -> a -> a
apply mon a1 a2 = mon.op a1 a2

type WithMinimum a
  = MinusInfinity
  |Normal a

biggest: WithMinimum comparable -> WithMinimum comparable -> WithMinimum comparable
biggest first second =
    case (first, second) of
      (MinusInfinity,  s)-> s
      (f, MinusInfinity) -> f
      (Normal f, Normal s) -> Normal (max f s)

maximum : Monoid ( WithMinimum comparable)
maximum = {zero = MinusInfinity, op=biggest}

int: Monoid Int
int = {zero = 0, op = (+)}

float : Monoid Float
float = {zero = 0.0, op =(+)}

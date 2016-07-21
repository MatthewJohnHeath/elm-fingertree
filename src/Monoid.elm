module Monoid exposing(
  Monoid,  int, appendZero)
{-| Module defining a monoid on a type and some functions for creatinbg them
# Definition
@docs Monoid

# Create
@docs appendZero, int

-}
{-|Contains a binary operator op : a -> a-> a such that op (op a b) c) = op a (op b c)
(associativity) and an element zero of type a such that for any a' of type a
op a' zero = op zero a' = a'-}
type alias Monoid a = {zero: a , op : a -> a -> a }
{-|Contains a binary operator op : a -> a-> a  that should satisfy  op (op a b) c) = op a (op b c)
(associativity) and an element zero of type a that should satisfy "for any a' of type a
op a' zero = op zero a' = a' "-}
type WithAppendedZero a
  = Zero
  |Normal a
{-|Given an associative function a->a->a extends this to a with an extra "zero" element to produce a monoid-}
appendZero: (a-> a -> a) -> Monoid (WithAppendedZero a)
appendZero op =
  let newOp a1 a2 =
    case (a1,a2) of
    (Zero,  a) -> a
    (a, Zero) -> a
    (Normal a, Normal a') -> Normal(op a a')
  in
  {zero = Zero , op = newOp}
{-|The monoid of integers with normal addition as the operation and 0 as zero -}
int: Monoid Int
int = {zero = 0, op = (+)}

module Internal exposing(..)

import Finger exposing(Finger, Finger(One))
import Node exposing (Node, Node(..))
import Monoid exposing (Monoid)
import Maybe
import Tuple exposing (second)

type InternalFingerTree a b
  =Empty
  |Single a
  |Deep (Finger a) ( InternalFingerTree (Node a, b) b)  b (Finger a)

empty: InternalFingerTree a b
empty = Empty

reduce: Monoid(b) ->  (a -> b) -> InternalFingerTree a b -> b
reduce monoid projection fingerTree=
  let  f = monoid.op << projection  in
  case fingerTree of
    Empty -> monoid.zero
    Single a0 -> f a0 monoid.zero
    Deep left _ annotation right ->
      let leftReduced = Finger.reduce f monoid.zero left in
      let midReduced = monoid.op annotation leftReduced  in
      Finger.reduce f midReduced right

ofFinger:  b ->  Finger.Finger a -> InternalFingerTree a b
ofFinger zero finger =
  case finger of
    Finger.One a1 -> Single a1
    Finger.Two a1 a2 ->
      Deep (Finger.One a1) Empty zero (Finger.One a2)
    Finger.Three a1 a2 a3 ->
      Deep (Finger.Two a1 a2) Empty zero (Finger.One a3)
    Finger.Four a1 a2 a3 a4 ->
      Deep (Finger.Two a1 a2) Empty zero (Finger.Two a3 a4)

leftPush:  Monoid b -> (a -> b) -> a -> InternalFingerTree a b -> InternalFingerTree a b
leftPush  monoid projection a0 fingerTree=
  let zero = monoid.zero in
  case fingerTree of
    Empty -> Single a0
    Single a1 ->
      Deep (Finger.One a0) Empty zero (Finger.One a1)
    Deep (Finger.Four a1 a2 a3 a4) middle _ right ->
      let node = Node.Node3 a2 a3 a4 in
      let nodeAnnotation =
        Node.reduce (monoid.op << projection) zero node in
      let midTree = leftPush monoid second (node, nodeAnnotation) middle in
      let midAnnotation = reduce monoid second midTree in
      Deep (Finger.Two a0 a1) midTree midAnnotation right
    Deep left middle annotation right ->
      Deep (Finger.leftAdd a0 left) middle annotation right

leftPeak: InternalFingerTree a b -> Maybe.Maybe a
leftPeak fingerTree =
  case fingerTree of
    Empty -> Nothing
    Single a -> Just a
    Deep left _ _ _ -> Just (Finger.leftPeak left)

leftPop: Monoid b -> InternalFingerTree a b-> InternalFingerTree a b
leftPop mon fingerTree =
  case fingerTree of
    Empty -> Empty
    Single _ -> Empty
    Deep (Finger.One a ) mid _ right ->
      case leftPeak mid of
      Just (node,_) ->
        let newMid = leftPop mon mid in
        Deep (Node.toFinger node) newMid (reduce mon second newMid) right
      Nothing -> ofFinger mon.zero right
    Deep left mid annotation right ->
      Deep (Finger.leftRemove left) mid annotation right

rightPush:  Monoid b -> (a -> b) -> a -> InternalFingerTree a b -> InternalFingerTree a b
rightPush  monoid projection a0 fingerTree=
  let zero = monoid.zero in
  case fingerTree of
    Empty -> Single a0
    Single a1 ->
      Deep (Finger.One a1) Empty zero (Finger.One a0)
    Deep  left middle _ (Finger.Four a4 a3 a2 a1) ->
      let node = Node.Node3 a4 a3 a2 in
      let nodeAnnotation =
        Node.reduce (monoid.op << projection) zero node in
      let midTree = rightPush monoid second (node, nodeAnnotation) middle in
      let midAnnotation = reduce monoid second midTree in
      Deep left midTree midAnnotation (Finger.Two a1 a0)
    Deep left middle annotation right ->
      Deep left middle annotation (Finger.rightAdd a0 right)

rightPeak: InternalFingerTree a b -> Maybe.Maybe a
rightPeak fingerTree =
  case fingerTree of
    Empty -> Nothing
    Single a -> Just a
    Deep _ _ _ right -> Just (Finger.rightPeak right)

rightPop: Monoid b -> InternalFingerTree a b-> InternalFingerTree a b
rightPop mon fingerTree =
  case fingerTree of
    Empty -> Empty
    Single _ -> Empty
    Deep left  mid _ (Finger.One a ) ->
      case rightPeak mid of
      Just (node,_) ->
        let newMid = rightPop mon mid in
        Deep left newMid (reduce mon second newMid) (Node.toFinger node)
      Nothing -> ofFinger mon.zero left
    Deep left mid annotation right ->
      Deep left mid annotation (Finger.rightRemove right)

ofList: Monoid(b) -> (a -> b) -> List a -> InternalFingerTree a b
ofList monoid projection list =
  List.foldl (rightPush monoid projection) empty list

-- Concatanation
nodify:  List a -> List (Node a)
nodify list=
  case list of
    [] -> []
    [_] -> []
    [a1,a2] -> [Node.Node2 a1 a2]
    [a1,a2,a3]-> [Node.Node3 a1 a2 a3]
    a1::a2::tail-> (Node.Node2 a1 a2) :: nodify tail

concatWithMiddle :  Monoid b -> (a -> b) -> InternalFingerTree a b -> List a -> InternalFingerTree a b -> InternalFingerTree a b
concatWithMiddle monoid projection left middle right =
  let rPush = rightPush monoid projection in
  let lPush = leftPush monoid projection in
  let annotate node =
    (node, Node.reduce (projection>>monoid.op) monoid.zero node)
  in
  let loop l m r =
    case (l, m, r) of
      (Empty, [], right) -> right
      (Empty, head::tail, right) -> lPush head (loop Empty tail right)
      (Single a, middle, right) -> lPush a (loop Empty middle right)
      (left, middle, Empty ) ->
          case middle |> List.reverse of
            [] -> left
            head::tail -> rPush head (loop left (List.reverse tail) Empty)
      (left, middle, Single a) -> rPush a (loop left middle Empty)
      ( (Deep lleft lmid lnote lright),
        middle,
        (Deep rleft rmid rnote rright) ) ->
        let newMidList =
          ((Finger.toList lright) ++ middle ++ (Finger.toList rleft))
          |> nodify
          |> List.map annotate
        in
        let newMidTree = concatWithMiddle monoid second lmid newMidList rmid in
        Deep lleft newMidTree (reduce monoid second newMidTree) rright
  in
  loop left middle right

concat: Monoid b -> (a -> b) -> InternalFingerTree a b ->  InternalFingerTree a b -> InternalFingerTree a b
concat monoid projection left right =
  concatWithMiddle monoid projection left [] right
-- Splitting
deepLeft : Monoid b  -> Finger a ->  InternalFingerTree (Node a, b) b -> b -> List a -> InternalFingerTree a b
deepLeft mon left mid annotation right =
  case Finger.tryFromList right of
    Just finger -> Deep left mid annotation finger
    Nothing ->
        case rightPeak mid of
          Just(newright,_)->
            let newmid = rightPop mon mid in
            Deep left newmid (reduce mon second newmid) (Node.toFinger newright)
          Nothing -> ofFinger mon.zero left

deepRight:Monoid b  -> List a ->  InternalFingerTree (Node a, b) b -> b -> Finger a -> InternalFingerTree a b
deepRight mon left mid annotation right =
  case Finger.tryFromList left of
    Just finger -> Deep finger mid annotation right
    Nothing ->
        case leftPeak mid of
          Just(newleft,_)->
            let newmid = leftPop mon mid in
            Deep (Node.toFinger newleft) newmid (reduce mon second newmid) right
          Nothing -> ofFinger mon.zero right

type alias Split a b =
  {
  left: InternalFingerTree a b ,
  middle: a,
  right: InternalFingerTree a b
  }

split : Monoid(b) -> (a -> b) -> (b -> Bool) -> b ->  InternalFingerTree a b ->  Maybe (Split a b)
split monoid projection measurement start tree =
  let folder a b = monoid.op b (projection a) in
  let red = reduce monoid second in
  let deepL = deepLeft monoid in
  let deepR = deepRight monoid in
  let fromList = ofList monoid projection in
    case tree of
     Single a ->
        if start |> folder a |> measurement then
          Just {left = Empty, middle = a, right = Empty }
        else Nothing
     Deep leftfinger mid annotation rightfinger ->
        case Finger.split folder measurement start leftfinger of
          Just leftsplit ->
            Just{
              left = fromList leftsplit.left,
              middle = leftsplit.mid,
              right = deepR leftsplit.right mid annotation rightfinger}
          Nothing ->
            let leftReduced = Finger.reduce folder start leftfinger in
            case split monoid second measurement leftReduced mid of
              Just midsplit ->
                let (node, _) = midsplit.middle in
                let upToNode = monoid.op leftReduced (red midsplit.left) in
                let splitNode = Node.split upToNode folder measurement node in
                let splitLeft = deepL leftfinger midsplit.left (red midsplit.left) splitNode.left
                in
                let splitRight = deepR splitNode.right midsplit.right (red midsplit.right) rightfinger
                in
                Just {
                  left = splitLeft,
                  middle = splitNode.mid,
                  right = splitRight}
              Nothing ->
                let midReduced = monoid.op leftReduced annotation in
                case Finger.split folder measurement midReduced rightfinger  of
                  Just rightsplit ->
                    Just{
                        left = deepL leftfinger  mid annotation rightsplit.left,
                        middle = rightsplit.mid,
                        right = fromList rightsplit.right }
                  Nothing -> Nothing
     _ -> Nothing

module Quantity exposing (..)

type Unit =  MM | CM | M | KM | Litre | Millilitre

type Quantity = Quantity Float Unit | InvalidQuantity

convert : Quantity -> Quantity
convert q1 =
    case q1 of
    (Quantity _ MM) -> q1
    (Quantity l CM) -> convert (Quantity (l * 10) MM)
    (Quantity l M) -> convert (Quantity (l * 100) CM)
    (Quantity l KM) -> convert (Quantity (l * 1000) M)
    (Quantity _ Millilitre) -> q1
    (Quantity l Litre) -> convert (Quantity (l * 1000) Millilitre)
    _  -> InvalidQuantity

equals : Quantity -> Quantity -> Bool
equals q1 q2 = convert q1 == convert q2

add : Quantity -> Quantity -> Quantity
add q1 q2 =
    case (q1, q2) of
    ((Quantity l1 MM), (Quantity l2 MM)) -> (Quantity (l1 + l2) MM)
    ((Quantity l1 Millilitre), (Quantity l2 Millilitre)) -> (Quantity (l1 + l2) Millilitre)
    ((Quantity _ MM), (Quantity _ Millilitre)) -> InvalidQuantity
    (_, _) -> add (convert q1) (convert q2)
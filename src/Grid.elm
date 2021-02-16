module Grid exposing
    ( Grid
    , GridSize
    , from
    , set
    , canGo
    , GridPosition
    , Action(..)
    )

import Tuple exposing (first, second)
type Action
    = GoLeft | GoUp | GoRigth | GoDown
type alias Grid a = List (List a)
type alias GridSize = (Int, Int)
type alias GridPosition = (Int, Int)



from : (Int, Int) -> Grid Int
from (h, w) =
    (List.repeat h << List.repeat w) 0


set : a -> GridPosition -> Grid a -> Grid a
set value position =
    at (first position) <| at (second position) <| always value


at : Int -> (a -> a) -> List a -> List a
at n f l =
    List.append (List.take n l) <|
        case List.drop n l of
            [] ->
                []

            x :: xs ->
                f x :: xs


canGo: GridSize -> GridPosition -> Action -> Bool
canGo gridSize position action = 
    case action of
       GoDown -> (first position) < (first gridSize)
       GoUp -> (first position) > 0
       GoLeft -> (second position) > 0
       GoRigth -> (second position) < (second gridSize) 

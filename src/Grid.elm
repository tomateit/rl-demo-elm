module Grid exposing
    ( Grid
    , GridSize
    , from
    , set
    , canGo
    )

import Agent exposing (Action)
import Tuple exposing (first, second)

type alias Grid a = List (List a)
type alias GridSize = (Int, Int)



from : (Int, Int) -> Grid Int
from (h, w) =
    (List.repeat h << List.repeat w) 0


set : a -> Int -> Int -> Grid a -> Grid a
set v x y =
    at y <| at x <| always v


at : Int -> (a -> a) -> List a -> List a
at n f l =
    List.append (List.take n l) <|
        case List.drop n l of
            [] ->
                []

            x :: xs ->
                f x :: xs


canGo: GridSize -> AgentPosition -> Action -> Bool
canGo gridSize position action = 
    case action of
       GoDown -> (first position) < (first gridSize)
       GoUp -> (first position) > 0
       GoLeft -> (second position) > 0
       GoRigth -> (second position) < (second gridSize) 

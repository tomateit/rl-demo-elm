module Agent exposing (Agent, go)
import Grid exposing (Action(..))
import Tuple exposing (first, second)

type alias GridPosition = (Int, Int)
type alias Agent = {
    health: Int,
    position: GridPosition,
    epsilon: Float, --exploration factor
    lr: Float -- learning rate
    }




-- moveAgent : Model -> Time.Posix -> Model
-- moveAgent model newTime = let
--                             newAgentPosition = (2, 3)
--                             matrixlist = Matrix.toLists model.state
    
--                             maybeNewState = Matrix.fromLists matrixlist
--                           in
--                             case maybeNewState of 
--                               Just newState ->
--                                   { model | time = newTime, agentPosition = newAgentPosition, state = newState }
--                               Nothing -> { model | time = newTime }

go: GridPosition -> Action -> GridPosition
go position action = 
    case action of 
        GoDown -> (first position + 1, second position)
        GoUp -> (first position - 1, second position)
        GoRigth -> (first position, second position + 1)
        GoLeft -> (first position, second position - 1)
module Agent exposing (Agent, AgentPosition, Action)
import Grid exposing (canGo)
import Html.Attributes exposing (action)
type alias Agent = {
    health: Int,
    position: AgentPosition,
    epsilon: Float, --exploration factor
    lr: Float -- learning rate
    }


type Action
    = GoLeft | GoUp | GoRigth | GoDown

type alias AgentPosition = (Int, Int)

go: AgentPosition -> Action -> AgentPosition
go position action = 
    case action of 
        GoDown -> (first position + 1, second position)
        GoUp -> (first position - 1, second position)
        GoRigth -> (first position, second position + 1)
        GoLeft -> (first position, second position - 1)
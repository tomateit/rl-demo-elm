module Agent exposing (Agent, AgentPosition)

type alias Agent = {
    health: Int,
    position: AgentPosition,
    epsilon: Float, --exploration factor
    lr: Float -- learning rate
    }

type alias AgentPosition = (Int, Int)
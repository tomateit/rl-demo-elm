module Main exposing (main)
import Browser
import Html exposing (Html, div, text, h3)
-- import Html.Events exposing (onClick, onInput)
-- import Html.Attributes exposing (attribute, class, id, type_, value)
import Html.Attributes exposing (class, id)
-- import Json.Encode exposing (encode)
import Random
-- import Task
import Time
-- import Task exposing (map)
-- import String exposing (join)
-- import Json.Decode exposing (maybe)
import Grid
import Grid exposing (GridSize)
import Agent exposing (Agent)
import Tuple exposing (first, second)
import String
-- import Grid exposing (..)
-- MODEL

type alias Model = {
  gridSize: GridSize,
  state: Grid.Grid Int,
  time: Time.Posix,
  acceleration: Int,
  agent: Agent,
  objective: Grid.GridPosition
  }

initialModel : () -> (Model, Cmd Msg)
initialModel _ = ({
  gridSize = (4, 6),
  state = Grid.from (4, 6),
  time = Time.millisToPosix 0,
  acceleration = 1,
  agent = Agent.Agent 1000 (1, 1) 0.95 0.05,
  objective = (4, 5)
  },
  Random.generate SetNewAgentPosition (randomPositionGenerator (4, 6)))


type Msg = SetNewAgentPosition (Int, Int)
  | Tick Time.Posix



randomPositionGenerator : (Int, Int) -> Random.Generator (Int, Int)
randomPositionGenerator gridSize = Random.pair (Random.int 0 (first gridSize)) (Random.int 0 (second gridSize))
   

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (model, Cmd.none)
    

view : Model -> Html Msg
view model = div [id "main"] [
  div [id "field", class "contentblob"] 
    (viewGrid model.state)
  ,
  div [id "controls", class "contentblob"]
    [ 
      showTime model
    ],
  div [
    id "info", 
    class "contentblob"] 
    [
      div [] [ text "Heigth: ", text (first model.gridSize |> String.fromInt ) ]
      , div [] [ text "Width: ", text (second model.gridSize |> String.fromInt) ]
      , div [] [ text "State: ", text (Debug.toString model.state ) ]
      , div [] [ text "Position: ", text (Debug.toString model.agent.position ) ]
    ]]

-- controllerpm: String -> Html
-- controllerpm fieldName = 
--   div [] [
--     button [ onClick decrementField fieldName ] [text "-"],
--     text  field fieldHeight model,
--     button [ onClick incrementField fieldName ] [text "+"],
--   ]


viewGrid:  Grid.Grid Int -> List(Html Msg)
viewGrid state = List.map (\gridRows -> List.map valToPad gridRows) state |> List.map wrapRow

valToPad: Int -> Html Msg
valToPad val = 
  div [class "pad"] [text ("["++ (String.fromInt val) ++ "]")]

wrapRow: List(Html Msg) -> Html Msg
wrapRow pads = 
  div [class "fieldrow"] pads
showTime : Model -> Html Msg
showTime model =
  let
    hour   = String.fromInt (Time.toHour   Time.utc model.time)
    minute = String.fromInt (Time.toMinute Time.utc model.time)
    second = String.fromInt (Time.toSecond Time.utc model.time)
  in
  h3 [] [ text (hour ++ ":" ++ minute ++ ":" ++ second) ]


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1000 Tick
main : Program () Model Msg
main =
    Browser.element
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

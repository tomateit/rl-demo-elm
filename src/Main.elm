module Main exposing (main)
import Browser
import Html exposing (Html, button, div, text, input, label, ul, li, h3)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (attribute, class, id, type_, value)
import Json.Encode exposing (encode)
import Random
import Task
import Time
import Task exposing (map)
import String exposing (join)
import Json.Decode exposing (maybe)
-- import Grid exposing (..)
-- MODEL

type alias Model = {
  gridSize: GridSize,
  state: Grid Int,
  time: Time.Posix,
  acceleration: Int,
  agentPosition: (Int, Int)
  }

initialModel : () -> (Model, Cmd Msg)
initialModel _ = ({
  gridSize = (4, 6),
  state = Matrix.initialize 4 6 (\_ -> 0),
  time = Time.millisToPosix 0,
  acceleration = 1,
  agentPosition = (0, 0)
  },
  Random.generate SetNewAgentPosition (randomPositionGenerator 4 6))

-- init : () -> (Model, Cmd Msg)
-- init _ =
--   ( Loading
--   , Http.get
--       { url = "https://elm-lang.org/assets/public-opinion.txt"
--       , expect = Http.expectString GotText
--       }
--   )
type Msg = IncrementFieldWidth 
  | DecrementFieldWidth
  | IncrementFieldHeight
  | DecrementFieldHeight
  | SetNewRandomField (List Int)
  | SetNewAgentPosition (Int, Int)
  | GenerateAgentPosition
  | GenerateNewFields
  | Tick Time.Posix


randomPositionGenerator : Int -> Int -> Random.Generator (Int, Int)
randomPositionGenerator heigth width =
   Random.pair (Random.int 0 heigth) (Random.int 0 width)
   
randomFieldGenerator : Int -> Random.Generator (List Int)
randomFieldGenerator count =
  Random.list count (Random.weighted (40, 1) [ (60, 0)])

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    IncrementFieldWidth ->
      if model.fieldWidth > 30 then (model, Cmd.none)
      else ({ 
        model | fieldWidth = model.fieldWidth + 1
      , state = (Matrix.initialize model.fieldHeight (model.fieldWidth + 1) (\_ -> 0))
       }, Cmd.none)
    DecrementFieldWidth ->
      if model.fieldWidth == 1 then (model, Cmd.none)
      else ({ 
        model | fieldWidth = model.fieldWidth - 1
      , state = (Matrix.initialize model.fieldHeight (model.fieldWidth - 1) (\_ -> 0))
       }, Cmd.none)
    IncrementFieldHeight -> let newHeigth = if model.fieldHeight < 30 then model.fieldHeight + 1 else model.fieldHeight
      in
      ({ model | fieldHeight = newHeigth
      , state = (Matrix.initialize (newHeigth) model.fieldWidth (\_ -> 0))
       }, Cmd.none)
    DecrementFieldHeight -> let newHeigth = if model.fieldHeight > 1 then model.fieldHeight - 1 else model.fieldHeight
      in
      ({ model | fieldHeight = newHeigth
      , state = (Matrix.initialize (newHeigth) model.fieldWidth (\_ -> 0))
       }, Cmd.none)
    GenerateNewFields -> (model, Random.generate SetNewRandomField (randomFieldGenerator (model.fieldHeight*model.fieldWidth) ))
    GenerateAgentPosition -> (model, Random.generate SetNewAgentPosition (randomPositionGenerator model.fieldHeight model.fieldWidth))
    SetNewRandomField values -> let h = first model.gridSize
                                    w = second model.gridSize 
                                    maybeNewState = Grid.fromList h w values
      in 
        case maybeNewState of 
          Just newState -> ({ model | state = newState}, Cmd.none)
          Nothing -> (model, Cmd.none)
    SetNewAgentPosition (row, col) -> ({model| agentPosition = (row, col)}, Cmd.none)
    Tick newTime ->
      ( moveAgent model newTime
      , Cmd.none
      )
      
    

view : Model -> Html Msg
view model = div [id "main"] [
  div [id "field", class "contentblob"] 
    (generateGrid model.state)
  ,
  div [id "controls", class "contentblob"]
    [ 
      showTime model,
      label [] [
        text "Field Width", 
        div [] [
          button [ onClick DecrementFieldWidth ] [text "-"],
          text  (String.fromInt model.fieldWidth),
          button [ onClick IncrementFieldWidth ] [text "+"]
        ]
    ]
    , label [] [
        text "Field Height", 
        div [] [
          button [ onClick DecrementFieldHeight ] [text "-"],
          text  (String.fromInt model.fieldHeight),
          button [ onClick IncrementFieldHeight ] [text "+"]
        ]
      ]
    , label [] [
        text "Seed", 
        div [] [
          button [ onClick GenerateNewFields ] [text "Generate randoms"]
        ]
      ]
      , label [] [
        text "New agent position", 
        div [] [
          button [ onClick GenerateAgentPosition ] [text "Randoms"]
        ]
      ]
    ],
  div [
    id "info", 
    class "contentblob"] 
    [
      div [] [ text "Heigth: ", text (String.fromInt model.fieldHeight) ]
      , div [] [ text "Width: ", text (String.fromInt model.fieldWidth) ]
      , div [] [ text "State: ", text (Matrix.pretty String.fromInt model.state) ]
      , div [] [ text "Position: ", text (Debug.toString model.agentPosition ) ]
    ]]

-- controllerpm: String -> Html
-- controllerpm fieldName = 
--   div [] [
--     button [ onClick decrementField fieldName ] [text "-"],
--     text  field fieldHeight model,
--     button [ onClick incrementField fieldName ] [text "+"],
--   ]
moveAgent : Model -> Time.Posix -> Model
moveAgent model newTime = let
                            newAgentPosition = (2, 3)
                            matrixlist = Matrix.toLists model.state
    
                            maybeNewState = Matrix.fromLists matrixlist
                          in
                            case maybeNewState of 
                              Just newState ->
                                  { model | time = newTime, agentPosition = newAgentPosition, state = newState }
                              Nothing -> { model | time = newTime }


showTime : Model -> Html Msg
showTime model =
  let
    hour   = String.fromInt (Time.toHour   Time.utc model.time)
    minute = String.fromInt (Time.toMinute Time.utc model.time)
    second = String.fromInt (Time.toSecond Time.utc model.time)
  in
  h3 [] [ text (hour ++ ":" ++ minute ++ ":" ++ second) ]
show:  Grid Int -> List(Html Msg)
show state = 
  (Matrix.map valToPad state) |> Matrix.toLists |> (List.map wrapRow)

valToPad: Int -> Html Msg
valToPad val = 
  div [class "pad"] [text ("["++ (String.fromInt val) ++ "]")]

wrapRow: List(Html Msg) -> Html Msg
wrapRow pads = 
  div [class "fieldrow"] pads

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

module Main exposing (main)
import Browser
import Html exposing (Html, button, div, text, input, label, ul, li, h3)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (attribute, class, id, type_, value)
import Json.Encode exposing (encode)
import Matrix exposing (Matrix, toList, fromList)
import Random
import Task
import Time
-- MODEL
type alias Model = {
  fieldWidth: Int,
  fieldHeight: Int,
  state: Matrix Int,
  time: Time.Posix,
  acceleration: Int
  }

initialModel : () -> (Model, Cmd Msg)
initialModel _ = ({
  fieldHeight = 4,
  fieldWidth = 6,
  state = Matrix.initialize 4 6 (\_ -> 0),
  time = Time.millisToPosix 0,
  acceleration = 1
  },
  Cmd.none)

-- init : () -> (Model, Cmd Msg)
-- init _ =
--   ( Loading
--   , Http.get
--       { url = "https://elm-lang.org/assets/public-opinion.txt"
--       , expect = Http.expectString GotText
--       }
--   )

-- randomInitializer : Int -> Random.Generator (List Int)
-- randomInitializer count =
--     Random. count (int 0 1)
type Msg = IncrementFieldWidth 
  | DecrementFieldWidth
  | IncrementFieldHeight
  | DecrementFieldHeight
  | NewRandomField (List Int)
  | GenerateNewFiels
  | Tick Time.Posix

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
    GenerateNewFiels -> (model, Random.generate NewRandomField (randomFieldGenerator (model.fieldHeight*model.fieldWidth) ))
    NewRandomField values -> let h = model.fieldHeight
                                 w = model.fieldWidth 
                                 maybeNewState = Matrix.fromList h w values
      in 
        case maybeNewState of 
          Just newState -> ({ model | state = newState}, Cmd.none)
          Nothing -> (model, Cmd.none)
    Tick newTime ->
      ( { model | time = newTime }
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
          button [ onClick GenerateNewFiels ] [text "Generate randoms"]
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
    ]]

-- controllerpm: String -> Html
-- controllerpm fieldName = 
--   div [] [
--     button [ onClick decrementField fieldName ] [text "-"],
--     text  field fieldHeight model,
--     button [ onClick incrementField fieldName ] [text "+"],
--   ]
showTime : Model -> Html Msg
showTime model =
  let
    hour   = String.fromInt (Time.toHour   Time.utc model.time)
    minute = String.fromInt (Time.toMinute Time.utc model.time)
    second = String.fromInt (Time.toSecond Time.utc model.time)
  in
  h3 [] [ text (hour ++ ":" ++ minute ++ ":" ++ second) ]
generateGrid:  Matrix Int -> List(Html Msg)
generateGrid state = 
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

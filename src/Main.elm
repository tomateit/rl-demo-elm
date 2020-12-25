module Main exposing (..)
import Browser
import Html exposing (Html, button, div, text, input, label)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (attribute, class, id, type_, value)
import Json.Encode exposing (encode)

main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL
type alias Model = {
  fieldWidth: String,
  fieldHeight: String
  }

init : Model
init = {
  fieldHeight = "4",
  fieldWidth = "6"
  }

type Msg = UpdateFieldWidth String 
  | UpdateFieldHeight String

update : Msg -> Model -> Model
update msg model =
  case msg of
    UpdateFieldWidth newWidth ->
      { model | fieldWidth = newWidth }
    UpdateFieldHeight newHeight ->
      { model | fieldHeight = newHeight }

view : Model -> Html Msg
view model = div [id "main"] [
  div [id "field", class "contentblob"] [],
  div [id "controls", class "contentblob"]
    [ label [] [
      text "Field Height", 
      input [ onInput UpdateFieldHeight, type_ "number",  value model.fieldHeight] []]
    , label [] [
      text "Field Width"
    , input [ onInput UpdateFieldWidth, type_ "number",  value model.fieldWidth ] []
    ]
    ],
  div [
    id "info", 
    class "contentblob"] 
    [
      div [] [ text "Heigth: ", text (model.fieldHeight) ]
      , div [] [ text "Width: ", text (model.fieldWidth) ]
    ]]


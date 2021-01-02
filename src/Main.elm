module Main exposing (main)
import Browser
import Html exposing (Html, button, div, text, input, label, ul, li)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (attribute, class, id, type_, value)
import Json.Encode exposing (encode)


-- MODEL
type alias Model = {
  fieldWidth: Int,
  fieldHeight: Int
  }

initialModel : Model
initialModel = {
  fieldHeight = 4,
  fieldWidth = 6
  }

type Msg = IncrementFieldWidth 
  | DecrementFieldWidth
  | IncrementFieldHeight
  | DecrementFieldHeight


update : Msg -> Model -> Model
update msg model =
  case msg of
    IncrementFieldWidth ->
      if model.fieldWidth > 30 then model
      else { model | fieldWidth = model.fieldWidth + 1 }
    DecrementFieldWidth ->
      if model.fieldWidth == 1 then model
      else { model | fieldWidth = model.fieldWidth - 1 }
    IncrementFieldHeight ->
      if model.fieldHeight > 30 then model
      else { model | fieldHeight = model.fieldHeight + 1 }
    DecrementFieldHeight ->
      if model.fieldHeight == 1 then model
      else { model | fieldHeight = model.fieldHeight - 1 }

view : Model -> Html Msg
view model = div [id "main"] [
  div [id "field", class "contentblob"] 
    (generateGrid (model.fieldHeight, model.fieldWidth))
  ,
  div [id "controls", class "contentblob"]
    [ 
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
    ],
  div [
    id "info", 
    class "contentblob"] 
    [
      div [] [ text "Heigth: ", text (String.fromInt model.fieldHeight) ]
      , div [] [ text "Width: ", text (String.fromInt model.fieldWidth) ]
    ]]

-- controllerpm: String -> Html
-- controllerpm fieldName = 
--   div [] [
--     button [ onClick decrementField fieldName ] [text "-"],
--     text  field fieldHeight model,
--     button [ onClick incrementField fieldName ] [text "+"],
--   ]

generateGrid:  (Int, Int) -> List(Html Msg)
generateGrid (heigth, width) = 
  List.repeat heigth (
    div [class "fieldrow"] (
      List.repeat width (div [class "pad"] [text "[]"])
    )
  )



main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }

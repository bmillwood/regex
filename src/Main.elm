module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Parser

import Regex exposing (Regex)
import Regex.Explain
import Regex.Parser

type alias Model =
  { unparsed : String
  , lastParsed : Regex
  , error : Maybe (List Parser.DeadEnd)
  }

type Msg
  = SetInput String
  | SetRegex Regex

init : Model
init =
  { unparsed = ""
  , lastParsed = Regex.empty
  , error = Nothing
  }

view : Model -> Html Msg
view model =
  let
    maybeStyle =
      case model.error of
        Just _ -> [ Attributes.style "background-color" "#fcc" ]
        Nothing -> []
    inputAttributes =
      [ Attributes.type_ "text"
      , Attributes.value model.unparsed
      , Events.onInput SetInput
      ] ++ maybeStyle
  in
  Html.div []
    [ Html.p [] [ Html.input inputAttributes [] ]
    , Html.p [] [ Regex.Explain.explainRegex model.lastParsed ]
    ]

update : Msg -> Model -> Model
update msg model =
  case msg of
    SetInput input ->
      case Parser.run Regex.Parser.parser input of
        Ok newRegex ->
          { unparsed = input, lastParsed = newRegex, error = Nothing }
        Err error ->
          { unparsed = input
          , lastParsed = model.lastParsed
          , error = Just error
          }
    SetRegex regex ->
      { unparsed = Regex.toString regex
      , lastParsed = regex
      , error = Nothing
      }

main =
  Browser.sandbox
    { init          = init
    , view          = view
    , update        = update
    }

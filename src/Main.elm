module Main exposing (main)

import Browser
import Debug
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
  = Input String

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
      , Events.onInput Input
      ] ++ maybeStyle
  in
  Html.div []
    [ Html.p [] [ Html.input inputAttributes [] ]
    , Html.p [] [ Regex.Explain.explainRegex model.lastParsed ]
    ]

update : Msg -> Model -> Model
update (Input input) model =
  case Parser.run Regex.Parser.parser input of
    Ok newRegex ->
      { unparsed = input, lastParsed = newRegex, error = Nothing }
    Err error ->
      { unparsed = input
      , lastParsed = model.lastParsed
      , error = Just error
      }

main =
  Browser.sandbox
    { init          = init
    , view          = view
    , update        = update
    }

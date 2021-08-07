module Main exposing (main)

import Browser
import Debug
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Parser

import Regex exposing (Regex)

type alias Model =
  { unparsed : String
  , parsed : Result (List Parser.DeadEnd) Regex
  }

type Msg
  = Input String

modelOfUnparsed : String -> Model
modelOfUnparsed unparsed =
  { unparsed = unparsed
  , parsed = Parser.run Regex.parser unparsed
  }

parsedToString : Result (List Parser.DeadEnd) Regex -> String
parsedToString = Debug.toString

init : Model
init = modelOfUnparsed ""

view : Model -> Html Msg
view model =
  Html.div []
    [ Html.p []
        [ Html.input
            [ Attributes.type_ "text"
            , Attributes.name "regex"
            , Attributes.value model.unparsed
            , Events.onInput Input
            ]
            []
        ]
    , Html.p []
        [ Html.text (parsedToString model.parsed)
        ]
    ]

update : Msg -> Model -> Model
update (Input input) _ = modelOfUnparsed input

main =
  Browser.sandbox
    { init          = init
    , view          = view
    , update        = update
    }

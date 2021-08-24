module Real exposing (Model, Msg, init, view, update)

import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Parser
import Url.Builder

import Regex exposing (Regex)
import Regex.Explain
import Regex.Parser
import Regex.Unparser

type alias Model =
  { unparsed : String
  , lastParsed : Regex
  , error : Maybe (List Parser.DeadEnd)
  }

type Msg
  = SetInput String
  | SetRegex Regex

init : { initialInput : String } -> (Model, Cmd Msg)
init { initialInput } =
  let
    emptyModel =
      { unparsed = ""
      , lastParsed = Regex.empty
      , error = Nothing
      }
  in
  update (SetInput initialInput) emptyModel

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
      , Attributes.placeholder "regular expression"
      , Events.onInput SetInput
      ] ++ maybeStyle
  in
  Html.div []
    [ Html.p
        []
        [ Html.input inputAttributes []
        , Html.text " "
        , Html.a
            [ Attributes.href
              <| Url.Builder.relative [] [ Url.Builder.string "init" model.unparsed ]
            ]
            [ Html.text "link to this regex" ]
        ]
    , Html.p [] [ Html.map SetRegex (Regex.Explain.explainRegex model.lastParsed) ]
    , Html.p
        []
        [ Html.hr [] []
        , Html.a
            [ Attributes.href "https://github.com/bmillwood/regex" ]
            [ Html.text "Source" ]
        ]
    ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetInput input ->
      case Parser.run Regex.Parser.parser input of
        Ok newRegex ->
          ( { unparsed = input, lastParsed = newRegex, error = Nothing }
          , Cmd.none
          )
        Err error ->
          ( { unparsed = input
            , lastParsed = model.lastParsed
            , error = Just error
            }
          , Cmd.none
          )
    SetRegex regex ->
      ( { unparsed = Regex.Unparser.toString regex
        , lastParsed = regex
        , error = Nothing
        }
      , Cmd.none
      )

module Test exposing (Model, Msg, init, view, update)

import Html exposing (Html)
import Parser exposing (Parser)
import Random

import Regex exposing (Regex)
import Regex.Gen
import Regex.Parser

type alias Model = { fuzzCases : List Regex }
type Msg = SetCases (List Regex)

init : (Model, Cmd Msg)
init =
  ( { fuzzCases = [] }
  , Random.generate SetCases
      (Random.list 10 (Regex.Gen.regex { size = 10 }))
  )

update : Msg -> Model -> (Model, Cmd Msg)
update (SetCases cases) model =
  ( { model | fuzzCases = cases }
  , Cmd.none
  )

regressionTests : List Regex
regressionTests =
  [ [[ Regex.Repeat (Regex.Repeat (Regex.Capture Regex.empty) Regex.ZeroOrMore) Regex.ZeroOrMore ]]
  , [[ Regex.CharMatching Regex.MatchAny, Regex.CharMatching (Regex.MatchLit '?') ]]
  , [[ Regex.Repeat (Regex.Capture Regex.empty) (Regex.Range { min = Nothing, max = Nothing }) ]]
  ]

type alias TestRow a =
  { pass : Html a
  , input : Html a
  , toString : Html a
  , parsed : Html a
  , reString : Html a
  }

toRowGen : (List (Html a) -> Html a) -> TestRow a -> Html a
toRowGen mkCell { pass, input, toString, parsed, reString } =
  Html.tr
    []
    [ mkCell [ pass ]
    , mkCell [ input ]
    , mkCell [ toString ]
    , mkCell [ parsed ]
    , mkCell [ reString ]
    ]

view : Model -> Html Msg
view { fuzzCases } =
  let
    header =
      toRowGen
        (Html.th [])
        { pass = Html.text "pass?"
        , input = Html.text "input"
        , toString = Html.text "string"
        , parsed = Html.text "parsed"
        , reString = Html.text "restring"
        }

    toRow input = toRowGen (Html.td []) (testRegex input)

    table cases =
      Html.table
        []
        [ Html.thead [] [ header ]
        , Html.tbody [] (List.map toRow cases)
        ]
  in
  Html.div
    []
    [ Html.h1 [] [ Html.text "Fuzz tests" ]
    , table fuzzCases
    , Html.h1 [] [ Html.text "Regression tests" ]
    , table regressionTests
    ]

testRegex : Regex -> TestRow a
testRegex regex =
  let
    input = Html.text (Debug.toString regex)
    toString = Regex.toString regex
    parseResult = Parser.run Regex.Parser.parser toString
    (parsed, reString) =
      case parseResult of
        Err _ ->
          ( Html.text "#ERR", "#ERR" )
        Ok newRegex ->
          ( Html.text (Debug.toString newRegex)
          , Regex.toString newRegex
          )
    pass =
      if parseResult == Ok regex && toString == reString
      then Html.text "yes"
      else Html.text "no"
  in
  { pass = pass
  , input = input
  , toString = Html.text toString
  , parsed = parsed
  , reString = Html.text reString
  }

module Test exposing (Model, Msg, init, view, update)

import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Parser exposing (Parser)
import Random

import Regex exposing (Regex)
import Regex.Gen
import Regex.Parser

type alias Model = { fuzzCases : List Regex, hidePassing : Bool }
type Msg
  = SetCases (List Regex)
  | SetHidePassing Bool

init : (Model, Cmd Msg)
init =
  ( { fuzzCases = [], hidePassing = True }
  , Random.generate SetCases
      (Random.list 100 (Regex.Gen.regex { size = 10 }))
  )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    newModel =
      case msg of
        SetCases fuzzCases ->
          { model | fuzzCases = fuzzCases }
        SetHidePassing hidePassing ->
          { model | hidePassing = hidePassing }
  in
  (newModel, Cmd.none)

regressionTests : List Regex
regressionTests =
  let
    lit c = Regex.CharMatching (Regex.MatchLit c)
    zeroOrMore piece = Regex.Repeat piece Regex.ZeroOrMore
    classOfChars cs =
      Regex.CharMatching
        (Regex.MatchClass { negated = False, matchAtoms = (List.map Regex.ClassLit cs) })
  in
  [ [[ zeroOrMore (zeroOrMore (Regex.Capture Regex.empty)) ]]
  , [[ lit '.', lit '?', lit '*', lit '+', lit '{', lit '}' ]]
  , [[ Regex.Repeat (Regex.Capture Regex.empty) (Regex.Range { min = Nothing, max = Nothing }) ]]
  , [[ classOfChars ['\\', '-', ']'] ]]
  , [[ classOfChars ['^', 'x'] ]]
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
view { fuzzCases, hidePassing } =
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

    toRow input =
      Maybe.map
        (toRowGen (Html.td []))
        (testRegex { hidePassing = hidePassing } input)

    table title cases =
      let
        (rows, omitted) =
          List.foldr
            (\test (rowsAcc, omittedAcc) ->
              case toRow test of
                Nothing -> (rowsAcc, omittedAcc + 1)
                Just row -> (row :: rowsAcc, omittedAcc)
            )
            ([], 0)
            cases
        footer =
          if omitted > 0
          then
            [ Html.p []
                [ Html.text (String.fromInt omitted)
                , Html.text " test"
                , Html.text (if omitted == 1 then "" else "s")
                , Html.text " not shown because "
                , Html.text (if omitted == 1 then "it" else "they")
                , Html.text " passed"
                ]
            ]
          else []
      in
      [ Html.h1 [] [ Html.text title ]
      , Html.table
          []
          [ Html.thead [] [ header ]
          , Html.tbody [] rows
          ]
      ] ++ footer
  in
  Html.div
    []
    (List.concat
      [ [ Html.form
            []
            [ Html.label
                [ Attributes.for "hidePassing"
                ]
                [ Html.input
                    [ Attributes.type_ "checkbox"
                    , Attributes.id "hidePassing"
                    , Attributes.checked hidePassing
                    , Events.onCheck SetHidePassing
                    ]
                    []
                , Html.text "Hide passing test cases"
                ]
            ]
        ]
      , table "Fuzz tests" fuzzCases
      , table "Regression tests" regressionTests
      ])

testRegex : { hidePassing : Bool } -> Regex -> Maybe (TestRow a)
testRegex { hidePassing } regex =
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
    isPassing = parseResult == Ok regex && toString == reString
  in
  if isPassing && hidePassing
  then Nothing
  else
    Just
      { pass = Html.text (if isPassing then "yes" else "no")
      , input = input
      , toString = Html.text toString
      , parsed = parsed
      , reString = Html.text reString
      }

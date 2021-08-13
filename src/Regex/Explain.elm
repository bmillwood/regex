module Regex.Explain exposing (..)

import Html exposing (Html)
import Parser

import Regex exposing (Regex)

explainRegex : Regex -> Html a
explainRegex regex = Html.p [] (explainDisjuncts regex)

explainDisjuncts : Regex -> List (Html a)
explainDisjuncts ds =
  case ds of
    [] -> [ Html.text "empty regex (matches nothing)" ]
    [ d ] -> explainDisjunct d
    _ ->
      [ Html.text "any of:"
      , Html.ul [] (List.map (Html.li [] << explainDisjunct) ds)
      ]

explainDisjunct : List Regex.Piece -> List (Html a)
explainDisjunct pieces =
  let
    explainSqueezed squeezed =
      case squeezed of
        Literals s ->
          [ Html.text ("the string " ++ Debug.toString s) ]
        Other piece -> explainPiece piece
  in
  case squeeze pieces of
    [] -> [ Html.text "the empty string" ]
    [ squeezed ] -> explainSqueezed squeezed
    squeezeds ->
      [ Html.text "a sequence of:"
      , Html.ul [] (List.map (Html.li [] << explainSqueezed) squeezeds)
      ]

type Squeezed
  = Literals String
  | Other Regex.Piece

squeeze : List Regex.Piece -> List Squeezed
squeeze =
  let
    f piece acc =
      case (piece, acc) of
        (Regex.CharMatching (Regex.MatchLit c), Literals s :: rest) ->
          Literals (String.cons c s) :: rest
        (Regex.CharMatching (Regex.MatchLit c), others) ->
          Literals (String.fromChar c) :: others
        (other, others) ->
          Other other :: others
  in
  List.foldr f []

explainPiece : Regex.Piece -> List (Html a)
explainPiece piece =
  case piece of
    Regex.StartOfInput -> [ Html.text "the start of the string" ]
    Regex.EndOfInput -> [ Html.text "the end of the string" ]
    Regex.CharMatching cm -> explainCharMatch cm
    Regex.Capture r -> explainDisjuncts r
    Regex.Repeat repetition unit ->
      explainRepetition repetition unit

explainCharMatch : Regex.CharMatch -> List (Html a)
explainCharMatch match =
  case match of
    Regex.MatchLit c ->
      -- NB. despite squeeze, this case is reachable, because
      -- we only bother squeezing when we have a sequence
      [ Html.text ("the character " ++ Debug.toString c) ]
    Regex.MatchAny -> [ Html.text "any character" ]
    Regex.MatchClass class -> explainMatchClass class

explainMatchClass : { negated : Bool, matchAtoms : List Regex.ClassAtom } -> List (Html a)
explainMatchClass { negated, matchAtoms } =
  let
    orText beforeEach beforeLast bits =
      case bits of
        [] -> []
        [ bit ] -> [ bit ]
        [ bit1, bit2 ] ->
          [ bit1, beforeEach, beforeLast, bit2 ]
        bit :: others ->
          bit :: beforeEach :: orText beforeEach beforeLast others
    explained =
      case partitionClassAtoms matchAtoms of
        (lits, ranges) ->
          List.concat
            [ List.map Html.text
                (orText ", " "or " (List.map Debug.toString lits))
            , if not (List.isEmpty lits || List.isEmpty ranges)
                then [ Html.text ", or " ]
                else []
            , explainRanges ranges
            ]
    explainRanges ranges =
      case ranges of
        [] -> []
        [ range ] ->
          (Html.text "in the range " :: explainRange range)
        _ ->
          List.concat (
              [ Html.text "in any of the ranges " ]
              :: orText [ Html.text ", " ] []
                  (List.map explainRange ranges)
            )
    explainRange (start, end) =
      [ Html.text (String.fromChar start)
      , Html.text "-"
      , Html.text (String.fromChar end)
      ]
  in
  [ Html.text "a character "
  , Html.text (if negated then "not " else "")
  ] ++ explained

partitionClassAtoms
  : List Regex.ClassAtom -> (List Char, List (Char, Char))
partitionClassAtoms =
  let
    f classAtom (lits, ranges) =
      case classAtom of
        Regex.ClassLit c -> (c :: lits, ranges)
        Regex.ClassRange c1 c2 -> (lits, (c1, c2) :: ranges)
  in
  List.foldr f ([], [])

explainRepetition : Regex.Piece -> Regex.Repetition -> List (Html a)
explainRepetition piece repetition =
  let
    (min, max) =
      case repetition of
        Regex.Optional -> (0, Just 1)
        Regex.ZeroOrMore -> (0, Nothing)
        Regex.OneOrMore -> (1, Nothing)
        Regex.Exactly n -> (n, Just n)
        Regex.Range range -> (Maybe.withDefault 0 range.min, range.max)
    prettyNumber n =
      case n of
        0 -> "zero"
        1 -> "one"
        _ -> String.fromInt n
    explanation = Html.ul [] [ Html.li [] (explainPiece piece) ]
  in
  case max of
    Nothing ->
      [ Html.text (prettyNumber min)
      , Html.text " or more of:"
      , explanation
      ]
    Just m ->
      if min == m
      then
        [ Html.text "exactly "
        , Html.text (prettyNumber min)
        , Html.text " of:"
        , explanation
        ]
      else if min == 0
      then
        [ Html.text "at most "
        , Html.text (prettyNumber m)
        , Html.text " of:"
        , explanation
        ]
      else
        [ Html.text "between "
        , Html.text (prettyNumber min)
        , Html.text " and "
        , Html.text (prettyNumber m)
        , Html.text (if m <= min then " (??)" else "")
        , Html.text " of:"
        , explanation
        ]

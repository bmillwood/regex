module Regex.Explain exposing (..)

import Debug
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

explainDisjunct : List Regex.Atom -> List (Html a)
explainDisjunct atoms =
  let
    explainSAtom satom =
      case satom of
        Literals s ->
          [ Html.text ("the string " ++ Debug.toString s) ]
        Other atom -> explainAtom atom
  in
  case squeezeAtoms atoms of
    [] -> [ Html.text "the empty string" ]
    [ satom ] -> explainSAtom satom
    satoms ->
      [ Html.text "a sequence of:"
      , Html.ul [] (List.map (Html.li [] << explainSAtom) satoms)
      ]

type SqueezedAtom
  = Literals String
  | Other Regex.Atom

squeezeAtoms : List Regex.Atom -> List SqueezedAtom
squeezeAtoms =
  let
    f atom acc =
      case (atom, acc) of
        (Regex.Literal c, Literals s :: rest) ->
          Literals (String.cons c s) :: rest
        (Regex.Literal c, others) ->
          Literals (String.fromChar c) :: others
        (other, others) ->
          Other other :: others
  in
  List.foldr f []

explainAtom : Regex.Atom -> List (Html a)
explainAtom atom =
  case atom of
    Regex.Literal c ->
      -- NB. despite squeezeAtoms, this case is reachable, because
      -- we only bother squeezing when we have a sequence
      [ Html.text ("the character " ++ Debug.toString c) ]
    Regex.CharacterClass cc -> explainCharacterClass cc
    Regex.Capture r -> explainDisjuncts r
    Regex.Repeat repetition unit ->
      explainRepetition repetition unit

explainCharacterClass : { negated : Bool, atoms : List Regex.CharClassAtom } -> List (Html a)
explainCharacterClass { negated, atoms } =
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
      case partitionCCAtoms atoms of
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

partitionCCAtoms
  : List Regex.CharClassAtom -> (List Char, List (Char, Char))
partitionCCAtoms =
  let
    f ccatom (lits, ranges) =
      case ccatom of
        Regex.CCLiteral c -> (c :: lits, ranges)
        Regex.CCRange c1 c2 -> (lits, (c1, c2) :: ranges)
  in
  List.foldr f ([], [])

explainRepetition : Regex.Repetition -> Regex.Atom -> List (Html a)
explainRepetition { min, max } atom =
  let
    prettyNumber n =
      case n of
        0 -> "zero"
        1 -> "one"
        _ -> String.fromInt n
    explanation = Html.ul [] [ Html.li [] (explainAtom atom) ]
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
        , Html.text " of:"
        , explanation
        ]
module Regex.Explain exposing (..)

import Debug
import Html exposing (Html)
import Parser

import Regex exposing (Regex)

explainRegex : Regex -> Html a
explainRegex regex =
  let
    explainDisjuncts ds =
      case ds of
        [] -> [ Html.text "empty regex (matches nothing)" ]
        [ d ] -> explainDisjunct d
        _ ->
          [ Html.text "one of:"
          , Html.ul [] (List.map (Html.li [] << explainDisjunct) ds)
          ]
    explainDisjunct atoms =
      case squeezeAtoms atoms of
        [] -> [ Html.text "the empty string" ]
        [ satom ] -> explainSAtom satom
        satoms ->
          [ Html.text "a sequence of:"
          , Html.ul [] (List.map (Html.li [] << explainSAtom) satoms)
          ]
    explainSAtom satom =
      case satom of
        Literals s ->
          [ Html.text ("the string " ++ Debug.toString s) ]
        Other (Regex.Literal c) -> explainUnimplemented c
        Other (Regex.CharacterClass cc) -> explainUnimplemented cc
        Other (Regex.Capture r) ->
          explainDisjuncts r
        Other (Regex.Repeat unit repetition) ->
          explainUnimplemented (unit, repetition)
    explainUnimplemented thing =
      [ Html.text (Debug.toString thing) ]
  in
  Html.p [] (explainDisjuncts regex)

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

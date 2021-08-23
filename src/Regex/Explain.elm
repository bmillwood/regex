module Regex.Explain exposing (..)

import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Parser

import Regex exposing (Regex)
import Zipper

explainRegex : Regex -> Html Regex
explainRegex regex = Html.p [] (explainDisjuncts regex)

list : (a -> Html a) -> List a -> List (Html (List a))
list f xs =
  List.map
    (\(b, x, a) -> Html.map (\y -> Zipper.toList (b, y, a)) (f x))
    (Zipper.ofList xs)

single : (a -> List (Html a)) -> a -> List (Html (List a))
single f x = List.map (Html.map (\y -> [ y ])) (f x)

explainDisjuncts : Regex -> List (Html Regex)
explainDisjuncts ds =
  case ds of
    [] -> [ Html.text "empty regex (matches nothing)" ]
    [ d ] -> single explainDisjunct d
    _ ->
      [ Html.text "any of:"
      , Html.ul [] (list (Html.li [] << explainDisjunct) ds)
      ]

explainDisjunct : List Regex.Piece -> List (Html (List Regex.Piece))
explainDisjunct pieces =
  let
    explainSqueezed : Squeezed -> List (Html Squeezed)
    explainSqueezed squeezed =
      case squeezed of
        Literals s ->
          [ Html.text "the string "
          , Html.input
              [ Attributes.type_ "text"
              , Attributes.value s
              , Events.onInput Literals
              , Attributes.style "width" (String.fromInt (String.length s) ++ "em")
              ]
              []
          ]
        Other piece -> List.map (Html.map Other) (explainPiece piece)
    unsqueezes = List.map (Html.map (List.concatMap unsqueeze))
  in
  case squeeze pieces of
    [] -> [ Html.text "the empty string" ]
    [ squeezed ] -> unsqueezes (single explainSqueezed squeezed)
    squeezeds ->
      [ Html.text "a sequence of:"
      , Html.ul
          []
          (unsqueezes (list (Html.li [] << explainSqueezed) squeezeds))
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

unsqueeze : Squeezed -> List Regex.Piece
unsqueeze sq =
  case sq of
    Literals s ->
      List.map
        (\c -> Regex.CharMatching (Regex.MatchLit c))
        (String.toList s)
    Other p -> [ p ]

explainPiece : Regex.Piece -> List (Html Regex.Piece)
explainPiece piece =
  case piece of
    Regex.StartOfInput -> [ Html.text "the start of the string" ]
    Regex.EndOfInput -> [ Html.text "the end of the string" ]
    Regex.CharMatching cm -> explainCharMatch cm
    Regex.Capture r -> List.map (Html.map Regex.Capture) (explainDisjuncts r)
    Regex.Repeat unit repetition ->
      explainRepetition unit repetition

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

type RepetitionKind
  = Optional
  | ZeroOrMore
  | OneOrMore
  | Exactly
  | AtLeast
  | Range

repetitionKind : Regex.Repetition -> RepetitionKind
repetitionKind rep =
  case rep of
    Regex.Optional -> Optional
    Regex.ZeroOrMore -> ZeroOrMore
    Regex.OneOrMore -> OneOrMore
    Regex.Exactly _ -> Exactly
    Regex.Range { min, max } ->
      case max of
        Nothing -> AtLeast
        Just _ -> Range

repetitionOfKind : { min : Int, max : Int } -> RepetitionKind -> Regex.Repetition
repetitionOfKind { min, max } kind =
  case kind of
    Optional -> Regex.Optional
    ZeroOrMore -> Regex.ZeroOrMore
    OneOrMore -> Regex.OneOrMore
    Exactly -> Regex.Exactly min
    AtLeast -> Regex.Range { min = Just min, max = Nothing }
    Range -> Regex.Range { min = Just min, max = Just max }

explainRepetition : Regex.Piece -> Regex.Repetition -> List (Html Regex.Piece)
explainRepetition piece repetition =
  let
    toIntOrZero s =
      if s == ""
      then Just 0
      else String.toInt s
    numberField n toRep =
      Html.input
        [ Attributes.type_ "text"
        , Attributes.value (String.fromInt n)
        , Events.onInput (\s ->
              updateRepetition (toRep (Maybe.withDefault n (toIntOrZero s)))
            )
        , Attributes.style "width" "3em"
        ]
        []
    prettyNumber n =
      case n of
        0 -> "zero"
        1 -> "one"
        _ -> String.fromInt n
    explanation =
      Html.ul [] [ Html.li [] (explainPiece piece) ]
      |> Html.map (\p -> Regex.Repeat p repetition)
    selectedKind = repetitionKind repetition
    kindsAndValues =
      [ ( Optional, "optionally" )
      , ( ZeroOrMore, "any number of" )
      , ( OneOrMore, "at least one of" )
      , ( Exactly, "exactly" )
      , ( AtLeast, "at least" )
      , ( Range, "between" )
      ]
    valueOf kind =
      case List.filter (\(k, v) -> k == kind) kindsAndValues of
        [ (_, v) ] -> v
        _ -> ""
    kindOf value =
      case List.filter (\(k, v) -> v == value) kindsAndValues of
        [ (k, _) ] -> k
        _ -> Optional
    repetitionOf kind =
      let
        bounds =
          case repetition of
            Regex.Optional -> { min = 0, max = 1 }
            Regex.ZeroOrMore -> { min = 0, max = 0 }
            Regex.OneOrMore -> { min = 1, max = 1 }
            Regex.Exactly n -> { min = n, max = n }
            Regex.Range { min, max } ->
              let
                newMin = Maybe.withDefault 0 min
              in
              { min = newMin
              , max = Maybe.withDefault newMin max
              }
      in
      repetitionOfKind bounds kind
    updateRepetition rep = Regex.Repeat piece rep
    optionOf kind =
      Html.option
        [ Attributes.selected (kind == selectedKind)
        ]
        [ Html.text (valueOf kind) ]
    repetitionSelect =
      Html.select
        [ Events.onInput (\s -> updateRepetition (repetitionOf (kindOf s)))
        ]
        [ optionOf Optional
        , optionOf ZeroOrMore
        , optionOf OneOrMore
        , optionOf Exactly
        , optionOf AtLeast
        , optionOf Range
        ]
    simple =
      [ repetitionSelect
      , explanation
      ]
  in
  case repetition of
    Regex.Optional -> simple
    Regex.ZeroOrMore -> simple
    Regex.OneOrMore -> simple
    Regex.Exactly n ->
      [ repetitionSelect
      , numberField n (\m -> Regex.Exactly m)
      , Html.text " of:"
      , explanation
      ]
    Regex.Range { min, max } ->
      case max of
        Nothing ->
          [ repetitionSelect
          , numberField
              (Maybe.withDefault 0 min)
              (\newMin -> Regex.Range { min = Just newMin, max = max })
          , Html.text " of:"
          , explanation
          ]
        Just m ->
          [ repetitionSelect
          , numberField
              (Maybe.withDefault 0 min)
              (\newMin -> Regex.Range { min = Just newMin, max = max })
          , Html.text " and "
          , numberField
              m
              (\newMax -> Regex.Range { min = min, max = Just newMax })
          , Html.text " of:"
          , explanation
          ]

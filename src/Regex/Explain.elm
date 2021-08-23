module Regex.Explain exposing (..)

import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Parser

import Regex exposing (Regex)
import Variant.Html
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

repetitionSelect : Variant.Html.Select Regex.Repetition
repetitionSelect =
  let
    toIntOrZero s =
      if s == ""
      then Just 0
      else String.toInt s
    numberField n =
      Html.input
        [ Attributes.type_ "text"
        , Attributes.value (String.fromInt n)
        , Events.onInput (\s -> Maybe.withDefault n (toIntOrZero s))
        , Attributes.style "width" "3em"
        ]
        []
    guessBounds rep =
      case rep of
        Regex.Optional -> { min = 0, max = 1 }
        Regex.ZeroOrMore -> { min = 0, max = 1 }
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
  [ Variant.Html.ofUnit "optionally" Regex.optional
  , Variant.Html.ofUnit "any number of" Regex.zeroOrMore
  , Variant.Html.ofUnit "at least one of" Regex.oneOrMore
  , Variant.Html.ofVariant
      "exactly"
      Regex.exactly
      (guessBounds >> .min)
      (\n ->
        [ numberField n
        , Html.text " of:"
        ]
      )
  , Variant.Html.ofVariant
      "at least"
      Regex.atLeast
      (guessBounds >> .min >> Just)
      (\min ->
        [ numberField (Maybe.withDefault 0 min)
          |> Html.map Just
        , Html.text " of:"
        ]
      )
  , Variant.Html.ofVariant
      "between"
      Regex.between
      (guessBounds >> (\{ min, max } -> (Just min, max)))
      (\(maybeMin, max) ->
          [ numberField (Maybe.withDefault 0 maybeMin)
            |> Html.map (\newMin -> (Just newMin, max))
          , Html.text " and "
          , numberField max
            |> Html.map (\newMax -> (maybeMin, newMax))
          , Html.text " of:"
          ]
      )
  ]


explainRepetition : Regex.Piece -> Regex.Repetition -> List (Html Regex.Piece)
explainRepetition piece repetition =
  List.concat
    [ Variant.Html.toHtml repetitionSelect repetition
      |> List.map (Html.map (\rep -> Regex.Repeat piece rep))
    , [ Html.ul [] [ Html.li [] (explainPiece piece) ]
        |> Html.map (\p -> Regex.Repeat p repetition)
      ]
    ]

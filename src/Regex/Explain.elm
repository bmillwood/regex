module Regex.Explain exposing (..)

import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Parser

import Regex exposing (Regex)
import Variant
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
    Regex.CharMatching cm -> List.map (Html.map Regex.CharMatching) (explainCharMatch cm)
    Regex.Capture r -> List.map (Html.map Regex.Capture) (explainDisjuncts r)
    Regex.Repeat unit repetition ->
      explainRepetition unit repetition

explainCharMatch : Regex.CharMatch -> List (Html Regex.CharMatch)
explainCharMatch match =
  case match of
    Regex.MatchLit c ->
      -- NB. despite squeeze, this case is reachable, because
      -- we only bother squeezing when we have a sequence
      [ Html.text ("the character " ++ Debug.toString c) ]
    Regex.MatchAny -> [ Html.text "any character" ]
    Regex.MatchClass class -> explainMatchClass class

classAtomSelect : Variant.Html.Select Regex.ClassAtom
classAtomSelect =
  let
    charField c =
      Html.input
        [ Attributes.type_ "text"
        , Attributes.value (String.fromChar c)
        , Events.onInput (\s ->
              case String.uncons (String.filter (\sc -> c /= sc) s) of
                Nothing -> c
                Just (newC, _) -> newC
            )
        , Attributes.style "width" "1em"
        ]
        []
    guessChar ca =
      case ca of
        Nothing -> 'a'
        Just (Regex.ClassLit c) -> c
        Just (Regex.ClassRange c1 _) -> c1
  in
  [ Variant.Html.ofVariant
      "equal to"
      Regex.classLit
      guessChar
      (\c ->
        [ Html.text " "
        , charField c
        ]
      )
  , Variant.Html.ofVariant
      "in the range"
      Regex.classRange
      (\ca -> let c = guessChar ca in (c, c))
      (\(c1, c2) ->
        [ Html.text " "
        , charField c1
          |> Html.map (\new1 -> (new1, c2))
        , Html.text " to "
        , charField c2
          |> Html.map (\new2 -> (c1, new2))
        ]
      )
  ]

explainMatchClass : { negated : Bool, matchAtoms : List Regex.ClassAtom } -> List (Html Regex.CharMatch)
explainMatchClass ({ negated, matchAtoms } as matchClass) =
  let
    ofAtom ca = Html.li [] (Variant.Html.toHtml classAtomSelect ca)
    setNegated newNegated = Regex.MatchClass { matchClass | negated = newNegated }
    setAtoms newAtoms = Regex.MatchClass { matchClass | matchAtoms = newAtoms }
  in
  List.concat
  [ [ Html.text "a character that is " ]
  , Variant.Html.toHtml
      [ Variant.Html.ofUnit "any" (Variant.unit False)
      , Variant.Html.ofUnit "none" (Variant.unit True)
      ]
      negated
    |> List.map (Html.map setNegated)
  , [ Html.text " of:"
    , Html.ul [] (list ofAtom matchAtoms)
      |> Html.map setAtoms
    ]
  ]

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
    guessBounds maybeRep =
      case maybeRep of
        Nothing -> { min = 0, max = 1 }
        Just Regex.Optional -> { min = 0, max = 1 }
        Just Regex.ZeroOrMore -> { min = 0, max = 1 }
        Just Regex.OneOrMore -> { min = 1, max = 1 }
        Just (Regex.Exactly n) -> { min = n, max = n }
        Just (Regex.Range { min, max }) ->
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

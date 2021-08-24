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

type EditListItem a
  = Set a
  | Insert a
  | Delete

insertButton : a -> Html (EditListItem a)
insertButton a =
  Html.button [ Events.onClick (Insert a) ] [ Html.text "+" ]

deleteButton : { disabled : Bool } -> Html (EditListItem a)
deleteButton { disabled } =
  Html.button
    [ Attributes.disabled disabled
    , Events.onClick Delete
    ]
    [ Html.text "-" ]

editableList : (a -> Html (EditListItem a)) -> List a -> List (Html (List a))
editableList f xs =
  List.map
    (\(b, x, a) ->
      Html.map
        (\y ->
          case y of
            Set z -> Zipper.toList (b, z, a)
            Insert z -> Zipper.toList (z :: b, x, a)
            Delete ->
              case (b, a) of
                (z :: bb, _) -> Zipper.toList (bb, z, a)
                (_, z :: aa) -> Zipper.toList (b, z, aa)
                ([], []) -> []
        )
        (f x)
    )
    (Zipper.ofList xs)

editableOne : (a -> List (Html (EditListItem a))) -> a -> List (Html (List a))
editableOne f x =
  List.map
    (Html.map
      (\y ->
        case y of
          Set z -> [ z ]
          Insert z -> [ z, x ]
          Delete -> []
      )
    )
    (f x)

explainDisjuncts : Regex -> List (Html Regex)
explainDisjuncts ds =
  case ds of
    [] -> [ Html.text "empty regex (matches nothing)" ]
    [ d ] -> editableOne explainDisjunct d
    _ ->
      [ Html.text "any of:"
      , Html.ul [] (editableList (Html.li [] << explainDisjunct) ds)
      ]

explainDisjunct : List Regex.Piece -> List (Html (EditListItem (List Regex.Piece)))
explainDisjunct pieces =
  let
    insertAnyButton =
      -- can't duplicate the current node because Literals nodes can't be
      -- adjacent to each other, so insert some arbitrary node
      insertButton (Other (Regex.CharMatching Regex.MatchAny))
    explainSqueezed : Squeezed -> List (Html (EditListItem Squeezed))
    explainSqueezed squeezed =
      case squeezed of
        Literals s ->
          [ insertAnyButton
          , deleteButton { disabled = False }
          , Html.text " the string "
          , Html.input
              [ Attributes.type_ "text"
              , Attributes.value s
              , Events.onInput (Set << Literals)
              , Attributes.style "width" (String.fromInt (String.length s) ++ "em")
              ]
              []
          ]
        Other piece ->
          insertAnyButton
          :: deleteButton { disabled = False }
          :: List.map (Html.map (Set << Other)) (explainPiece piece)
    unsqueezes = List.map (Html.map (Set << List.concatMap unsqueeze))
  in
  case squeeze pieces of
    [] -> [ Html.text "the empty string" ]
    [ squeezed ] -> unsqueezes (editableOne explainSqueezed squeezed)
    squeezeds ->
      [ Html.text "a sequence of:"
      , Html.ul
          []
          (unsqueezes
          <| editableList
              (Html.li [] << explainSqueezed)
              squeezeds
          )
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
    ofAtom ca =
      Html.li
        []
        (insertButton ca
        :: deleteButton { disabled = List.length matchAtoms <= 1 }
        :: List.map (Html.map Set) (Variant.Html.toHtml classAtomSelect ca)
        )
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
    , Html.ul [] (editableList ofAtom matchAtoms)
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

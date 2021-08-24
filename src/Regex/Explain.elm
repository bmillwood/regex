module Regex.Explain exposing (..)

import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Parser

import Regex exposing (Regex)
import Variant exposing (Variant)
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
    [ d ] -> editableOne (explainDisjunct { disableDelete = True }) d
    _ ->
      [ Html.text "any of:"
      , Html.ul [] (editableList (Html.li [] << explainDisjunct { disableDelete = False }) ds)
      ]

explainDisjunct
  :  { disableDelete : Bool }
  -> List Regex.Piece -> List (Html (EditListItem (List Regex.Piece)))
explainDisjunct { disableDelete } pieces =
  let
    insertDisjunctButton = Html.button [ Events.onClick (Insert []) ] [ Html.text "|" ]
    createSequenceButtons =
      [ Html.button [ Events.onClick (Insert arbitrary) ] [ Html.text "..." ] ]
    extendSequenceButtons =
      [ Html.button [ Events.onClick (Insert arbitrary) ] [ Html.text "+" ]
      , deleteButton { disabled = False }
      ]
    arbitrary =
      -- can't duplicate the current node because Literals nodes can't be
      -- adjacent to each other, so insert some arbitrary node
      Other (Regex.CharMatching Regex.MatchAny)
    unsqueezes = List.map (Html.map (Set << List.concatMap unsqueeze))
    one squeezed =
      editableOne
        (List.append createSequenceButtons << List.map (Html.map Set) << explainSqueezed)
        squeezed
      |> unsqueezes
  in
  insertDisjunctButton
  :: deleteButton { disabled = disableDelete }
  :: case squeeze pieces of
    [] -> one (Literals "")
    [ squeezed ] -> one squeezed
    squeezeds ->
      [ Html.text "a sequence of:"
      , Html.ul
          []
          (unsqueezes
          <| editableList
              (Html.li []
                << List.append extendSequenceButtons
                << List.map (Html.map Set)
                << explainSqueezed
              )
              squeezeds
          )
      ]

explainSqueezed : Squeezed -> List (Html Squeezed)
explainSqueezed squeezed =
  Variant.Html.toHtml squeezedSelect squeezed

type Squeezed
  = Literals String
  | Other Regex.Piece

emptyString : Variant Squeezed ()
emptyString = Variant.unit (Literals "")

literals : Variant Squeezed String
literals =
  { match = \s ->
      case s of
        Literals "" -> Nothing
        Literals l -> Just l
        _ -> Nothing
  , make = Literals
  }

other : Variant Squeezed Regex.Piece
other =
  { match = \s ->
      case s of
        Other p -> Just p
        _ -> Nothing
  , make = Other
  }

squeezedSelect : Variant.Html.Select Squeezed
squeezedSelect =
  [ Variant.Html.ofUnit "the empty string" emptyString
  , Variant.Html.ofVariant
      "the string"
      literals
      (always "a")
      (\s ->
        [ Html.text " "
        , Html.input
            [ Attributes.type_ "text"
            , Attributes.value s
            , Events.onInput identity
            , Attributes.style "width" (String.fromInt (String.length s) ++ "em")
            ]
            []
        ]
      )
  ] ++ List.map (Variant.Html.subOption other) pieceSelect

pieceSelect : Variant.Html.Select Regex.Piece
pieceSelect =
  [ Variant.Html.ofUnit "the start of input" Regex.startOfInput
  , Variant.Html.ofUnit "the end of input" Regex.endOfInput
  -- MatchLit deliberately omitted
  , Variant.Html.ofUnit
      "any character"
      (Variant.compose Regex.charMatching Regex.matchAny)
  , Variant.Html.ofVariant
      "a character that is"
      (Variant.compose Regex.charMatching Regex.matchClass)
      (always { negated = False, matchAtoms = [ Regex.ClassLit 'a' ] })
      explainMatchClass
  , Variant.Html.ofVariant
      "the captured result of"
      Regex.capture
      (always Regex.empty)
      explainDisjuncts
  , Variant.Html.ofVariant
      "[repetition]"
      Regex.repeat
      (always (Regex.CharMatching Regex.MatchAny, Regex.ZeroOrMore))
      explainRepetition
  ]

squeeze : List Regex.Piece -> List Squeezed
squeeze =
  let
    f piece acc =
      case (piece, acc) of
        (Regex.CharMatching (Regex.MatchLit c), Literals s :: rest) ->
          Literals (String.cons c s) :: rest
        (Regex.CharMatching (Regex.MatchLit c), others) ->
          Literals (String.fromChar c) :: others
        (otherP, others) ->
          Other otherP :: others
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

explainMatchClass
  :  { negated : Bool, matchAtoms : List Regex.ClassAtom }
  -> List (Html { negated : Bool, matchAtoms : List Regex.ClassAtom })
explainMatchClass ({ negated, matchAtoms } as matchClass) =
  let
    ofAtom ca =
      Html.li
        []
        (insertButton ca
        :: deleteButton { disabled = List.length matchAtoms <= 1 }
        :: List.map (Html.map Set) (Variant.Html.toHtml classAtomSelect ca)
        )
    setNegated newNegated = { matchClass | negated = newNegated }
    setAtoms newAtoms = { matchClass | matchAtoms = newAtoms }
  in
  List.concat
    [ Variant.Html.toHtml
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

explainRepetition : (Regex.Piece, Regex.Repetition) -> List (Html (Regex.Piece, Regex.Repetition))
explainRepetition (piece, repetition) =
  let
    explainPiece =
      squeeze [ piece ]
      |> List.concatMap explainSqueezed
      |> List.map (Html.map squeezedToPiece)

    squeezedToPiece sq =
      case unsqueeze sq of
        [ only ] -> only
        pieces -> Regex.Capture [ pieces ]
  in
  List.concat
    [ Variant.Html.toHtml repetitionSelect repetition
      |> List.map (Html.map (\rep -> (piece, rep)))
    , [ Html.ul [] [ Html.li [] explainPiece ]
        |> Html.map (\p -> (p, repetition))
      ]
    ]

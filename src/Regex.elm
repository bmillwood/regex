module Regex exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)

import Variant exposing (Variant)

type alias Regex = List (List Piece)

empty : Regex
empty = [[]]

type Piece
  = StartOfInput
  | EndOfInput
  | CharMatching CharMatch
  | Capture Regex
  | Repeat Piece Repetition

startOfInput : Variant Piece ()
startOfInput = Variant.unit StartOfInput

endOfInput : Variant Piece ()
endOfInput = Variant.unit EndOfInput

charMatching : Variant Piece CharMatch
charMatching =
  { match = \p ->
      case p of
        CharMatching m -> Just m
        _ -> Nothing
  , make = CharMatching
  }

capture : Variant Piece Regex
capture =
  { match = \p ->
      case p of
        Capture m -> Just m
        _ -> Nothing
  , make = Capture
  }

repeat : Variant Piece (Piece, Repetition)
repeat =
  { match = \p ->
      case p of
        Repeat i r -> Just (i, r)
        _ -> Nothing
  , make = \(p, r) -> Repeat p r
  }

type CharMatch
  = MatchLit Char
  | MatchAny
  | MatchClass { negated : Bool, matchAtoms : List ClassAtom }

matchLit : Variant CharMatch Char
matchLit =
  { match = \cm ->
      case cm of
        MatchLit c -> Just c
        _ -> Nothing
  , make = MatchLit
  }

matchAny : Variant CharMatch ()
matchAny = Variant.unit MatchAny

matchClass : Variant CharMatch { negated : Bool, matchAtoms : List ClassAtom }
matchClass =
  { match = \cm ->
      case cm of
        MatchClass c -> Just c
        _ -> Nothing
  , make = MatchClass
  }

type ClassAtom
  = ClassLit Char
  | ClassRange Char Char

classLit : Variant ClassAtom Char
classLit =
  { match = \r ->
      case r of
        ClassLit c -> Just c
        _ -> Nothing
  , make = ClassLit
  }

classRange : Variant ClassAtom (Char, Char)
classRange =
  { match = \r ->
      case r of
        ClassRange c1 c2 -> Just (c1, c2)
        _ -> Nothing
  , make = \(c1, c2) -> ClassRange c1 c2
  }

type Repetition
  = Optional
  | ZeroOrMore
  | OneOrMore
  | Exactly Int
  | Range
      { min : Maybe Int
      , max : Maybe Int
      }

optional : Variant Repetition ()
optional = Variant.unit Optional

zeroOrMore : Variant Repetition ()
zeroOrMore = Variant.unit ZeroOrMore

oneOrMore : Variant Repetition ()
oneOrMore = Variant.unit OneOrMore

exactly : Variant Repetition Int
exactly =
  { match = \r ->
      case r of
        Exactly n -> Just n
        _ -> Nothing
  , make = Exactly
  }

range : Variant Repetition { min : Maybe Int, max : Maybe Int }
range =
  { match = \r ->
      case r of
        Range ends -> Just ends
        _ -> Nothing
  , make = Range
  }

atLeast : Variant Repetition (Maybe Int)
atLeast =
  Variant.compose
    range
    { match = \ { min, max } ->
        case max of
          Nothing -> Just min
          Just _ -> Nothing
    , make = \min -> { min = min, max = Nothing }
    }

between : Variant Repetition (Maybe Int, Int)
between =
  Variant.compose
    range
    { match = \ { min, max } ->
        case max of
          Nothing -> Nothing
          Just m -> Just (min, m)
    , make = \(min, max) -> { min = min, max = Just max }
    }

--

reservedChars : Set Char
reservedChars =
  Set.fromList
    [ '\\'
    , '|'
    , '(', ')', '[', ']', '{', '}'
    , '^', '$', '.'
    , '?', '+', '*'
    ]

backslashEscapes : Dict Char Piece
backslashEscapes =
  Dict.fromList
    [ ('n', CharMatching (MatchLit '\n'))
    , ('r', CharMatching (MatchLit '\r'))
    ]

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

type CharMatch
  = MatchLit Char
  | MatchAny
  | MatchClass { negated : Bool, matchAtoms : List ClassAtom }

type ClassAtom
  = ClassLit Char
  | ClassRange Char Char

type Repetition
  = Optional
  | ZeroOrMore
  | OneOrMore
  | Exactly Int
  | Range
      { min : Maybe Int
      , max : Maybe Int
      }

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

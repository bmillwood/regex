module Regex exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)

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



module Regex exposing (..)

type alias Regex = List (List Atom)

empty : Regex
empty = [[]]

type Atom
  = Literal Char
  | CharacterClass { negated : Bool, atoms : List CharClassAtom }
  | Capture Regex
  | Repeat Atom Repetition

type CharClassAtom
  = CCLiteral Char
  | CCRange Char Char

type alias Repetition =
  { min : Maybe Int
  , max : Maybe Int
  }

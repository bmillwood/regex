module Regex exposing (..)

type alias Regex = List (List Atom)

empty : Regex
empty = [[]]

type Atom
  = Literal Char
  | CharacterClass { negated : Bool, atoms : List CharClassAtom }
  | Capture Regex
  | Repeat Repetition Atom

type CharClassAtom
  = CCLiteral Char
  | CCRange Char Char

type Repetition
  = Optional
  | ZeroOrMore
  | OneOrMore
  | Exactly Int
  | Range
      { min : Maybe Int
      , max : Maybe Int
      }

module Regex exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)

type alias Regex = List (List Atom)

empty : Regex
empty = [[]]

type Atom
  = StartOfInput
  | EndOfInput
  | CharMatching CharMatch
  | Capture Regex
  | Repeat Repetition Atom

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
  Set.fromList [ '\\', '|', '(', ')', '[', ']', '^', '$', '.' ]

backslashEscapes : Dict Char Atom
backslashEscapes =
  Dict.fromList
    [ ('n', CharMatching (MatchLit '\n'))
    , ('r', CharMatching (MatchLit '\r'))
    ]

charToString : Char -> String
charToString c =
  case c of
    '\n' -> "\\n"
    '\r' -> "\\r"
    other -> String.fromChar other

toString : Regex -> String
toString regex =
  case regex of
    [] -> "$."
    alts ->
      List.map (String.concat << List.map atomToString) alts
      |> String.join "|"

atomToString : Atom -> String
atomToString atom =
  case atom of
    StartOfInput -> "^"
    EndOfInput -> "$"
    CharMatching match -> charMatchToString match
    Capture c -> "(" ++ toString c ++ ")"
    Repeat r a ->
      let
        maybeIntToString =
          Maybe.withDefault "" << Maybe.map String.fromInt
        repeatString =
          case r of
            Optional -> "?"
            ZeroOrMore -> "*"
            OneOrMore -> "+"
            Exactly n -> "{" ++ String.fromInt n ++ "}"
            Range { min, max } ->
              String.concat
                [ "{"
                , maybeIntToString min
                , ","
                , maybeIntToString max
                , "}"
                ]
      in
      atomToString a ++ repeatString

charMatchToString : CharMatch -> String
charMatchToString cm =
  case cm of
    MatchLit c ->
      if Set.member c reservedChars
      then String.fromList [ '\\', c ]
      else charToString c
    MatchAny -> "."
    MatchClass { negated, matchAtoms } ->
      let
        ccAtomString ccatom =
          case ccatom of
            ClassLit c -> charToString c
            ClassRange c1 c2 ->
              charToString c1 ++ "-" ++ charToString c2
      in
      String.concat
        [ "["
        , if negated then "^" else ""
        , String.concat (List.map ccAtomString matchAtoms)
        , "]"
        ]

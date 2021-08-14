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

charToString : Char -> String
charToString c =
  case c of
    '\n' -> "\\n"
    '\r' -> "\\r"
    other -> String.fromChar other

escapeIf : Bool -> Char -> String
escapeIf b c =
  if b
  then String.fromList ['\\', c]
  else charToString c

toString : Regex -> String
toString regex =
  case regex of
    [] -> "$."
    alts ->
      List.map (String.concat << List.map pieceToString) alts
      |> String.join "|"

pieceToString : Piece -> String
pieceToString piece =
  case piece of
    StartOfInput -> "^"
    EndOfInput -> "$"
    CharMatching match -> charMatchToString match
    Capture c -> "(" ++ toString c ++ ")"
    Repeat a r ->
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
      pieceToString a ++ repeatString

mcEscapeworthy : Set Char
mcEscapeworthy =
  Set.fromList
    [ '\\'
    , '^', ']', '-'
    ]

charMatchToString : CharMatch -> String
charMatchToString cm =
  case cm of
    MatchLit c ->
      escapeIf (Set.member c reservedChars) c
    MatchAny -> "."
    MatchClass { negated, matchAtoms } ->
      let
        needEscape c = Set.member c mcEscapeworthy
        ccAtomString ccatom =
          case ccatom of
            ClassLit c -> escapeIf (needEscape c) c
            ClassRange c1 c2 ->
              String.concat
                [ escapeIf (needEscape c1) c1
                , "-"
                , escapeIf (needEscape c2) c2
                ]
      in
      String.concat
        [ "["
        , if negated then "^" else ""
        , String.concat (List.map ccAtomString matchAtoms)
        , "]"
        ]

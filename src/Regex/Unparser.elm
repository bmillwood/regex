module Regex.Unparser exposing (..)

import Set exposing (Set)

import Regex exposing (Regex)

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

pieceToString : Regex.Piece -> String
pieceToString piece =
  case piece of
    Regex.StartOfInput -> "^"
    Regex.EndOfInput -> "$"
    Regex.CharMatching match -> charMatchToString match
    Regex.Capture c -> "(" ++ toString c ++ ")"
    Regex.Repeat a r ->
      let
        maybeIntToString =
          Maybe.withDefault "" << Maybe.map String.fromInt
        repeatString =
          case r of
            Regex.Optional -> "?"
            Regex.ZeroOrMore -> "*"
            Regex.OneOrMore -> "+"
            Regex.Exactly n -> "{" ++ String.fromInt n ++ "}"
            Regex.Range { min, max } ->
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

charMatchToString : Regex.CharMatch -> String
charMatchToString cm =
  case cm of
    Regex.MatchLit c ->
      escapeIf (Set.member c Regex.reservedChars) c
    Regex.MatchAny -> "."
    Regex.MatchClass { negated, matchAtoms } ->
      let
        needEscape c = Set.member c mcEscapeworthy
        ccAtomString ccatom =
          case ccatom of
            Regex.ClassLit c -> escapeIf (needEscape c) c
            Regex.ClassRange c1 c2 ->
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

module Regex.Parser exposing (..)

import Dict exposing (Dict)
import Parser exposing (Parser, (|.), (|=))
import Set exposing (Set)

import Regex exposing (Regex)

parser : Parser Regex
parser =
  let
    eachBranch =
      Parser.sequence
        { start = ""
        , separator = ""
        , end = ""
        , spaces = Parser.succeed ()
        , item = pieceParser
        , trailing = Parser.Forbidden
        }
  in
  Parser.sequence
    { start = ""
    , separator = "|"
    , end = ""
    , spaces = Parser.succeed ()
    , item = eachBranch
    , trailing = Parser.Forbidden
    }

pieceParser : Parser Regex.Piece
pieceParser =
  let
    withoutRepetition =
      Parser.oneOf
        [ Parser.succeed Regex.StartOfInput
            |. Parser.symbol "^"
        , Parser.succeed Regex.EndOfInput
            |. Parser.symbol "$"
        , Parser.map Regex.CharMatching charMatch
        , Parser.succeed Regex.Capture
            |. Parser.symbol "("
            |= Parser.lazy (\ () -> parser)
            |. Parser.symbol ")"
        , backslashEscape
        ]
  in
  Parser.succeed (List.foldl (\rep piece -> Regex.Repeat piece rep))
    |= withoutRepetition
    |= repeats

type RepeatMax
  = This (Maybe Int)
  | ExactlyMin

repeats : Parser (List Regex.Repetition)
repeats =
  let
    repeat =
      Parser.oneOf
        [ Parser.succeed Regex.ZeroOrMore
            |. Parser.symbol "*"
        , Parser.succeed Regex.OneOrMore
            |. Parser.symbol "+"
        , Parser.succeed Regex.Optional
            |. Parser.symbol "?"
        , Parser.succeed identity
            |. Parser.symbol "{"
            |= parseNumberedRepeat
            |. Parser.symbol "}"
        ]
    parseNumberedRepeat =
      Parser.oneOf
        [ Parser.succeed (\m -> Regex.Range { min = Nothing, max = m })
            |. Parser.symbol ","
            |= Parser.oneOf [ Parser.map Just Parser.int, Parser.succeed Nothing ]
        , Parser.succeed applyMax
            |= Parser.int
            |= getMax
        ]
    applyMax min repeatMax =
      case repeatMax of
        This max -> Regex.Range { min = Just min, max = max }
        ExactlyMin -> Regex.Exactly min
    getMax =
      Parser.oneOf
        [ Parser.succeed This
            |. Parser.symbol ","
            |= Parser.oneOf
                [ Parser.succeed Just
                    |= Parser.int
                , Parser.succeed Nothing
                ]
        , Parser.succeed ExactlyMin
        ]
  in
  Parser.sequence
    { start = ""
    , separator = ""
    , end = ""
    , spaces = Parser.succeed ()
    , item = repeat
    , trailing = Parser.Forbidden
    }

charMatch : Parser Regex.CharMatch
charMatch =
  Parser.oneOf
    [ Parser.map Regex.MatchClass matchClass
    , Parser.succeed Regex.MatchAny
        |. Parser.symbol "."
    , Parser.map Regex.MatchLit plainLiteral
    ]

matchClass : Parser { negated : Bool, matchAtoms : List Regex.ClassAtom }
matchClass =
  let
    char =
      Parser.oneOf
        [ Parser.succeed identity
            |. Parser.symbol "\\"
            |= oneChar (\_ -> True)
        , oneChar (\c -> c /= ']')
        ]
    item =
      Parser.succeed (\c1 mc2 ->
        case mc2 of
          Nothing -> Regex.ClassLit c1
          Just c2 -> Regex.ClassRange c1 c2)
        |= char
        |= Parser.oneOf
            [ Parser.succeed Just
                |. Parser.symbol "-"
                |= char
            , Parser.succeed Nothing
            ]
    items =
      Parser.sequence
        { start = ""
        , separator = ""
        , end = "]"
        , spaces = Parser.succeed ()
        , item = item
        , trailing = Parser.Forbidden
        }
  in
  Parser.succeed (\negated atoms -> { negated = negated, matchAtoms = atoms })
    |. Parser.symbol "["
    |= Parser.oneOf
        [ Parser.succeed True
            |. Parser.symbol "^"
        , Parser.succeed False
        ]
    |= items

oneChar : (Char -> Bool) -> Parser Char
oneChar p =
  Parser.chompIf p
  |> Parser.getChompedString
  |> Parser.andThen (\s ->
        case String.uncons s of
          Nothing -> Parser.problem "no input"
          Just (c, _) -> Parser.succeed c)

plainLiteral : Parser Char
plainLiteral = oneChar (\c -> not (Set.member c Regex.reservedChars))

backslashEscape : Parser Regex.Piece
backslashEscape =
  Parser.succeed identity
    |. Parser.symbol "\\"
    |= oneChar (\_ -> True)
  |> Parser.andThen (\c ->
        if Set.member c Regex.reservedChars
        then Parser.succeed (Regex.CharMatching (Regex.MatchLit c))
        else case Dict.get c Regex.backslashEscapes of
          Just a -> Parser.succeed a
          Nothing -> Parser.problem "unrecognized backslash escape"
      )

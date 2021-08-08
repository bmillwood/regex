module Regex.Parser exposing (..)

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
        , item = atomParser
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

atomParser : Parser Regex.Atom
atomParser =
  let
    withoutRepetition =
      Parser.oneOf
        [ Parser.map Regex.CharacterClass characterClass
        , Parser.succeed Regex.Capture
            |. Parser.symbol "("
            |= Parser.lazy (\ () -> parser)
            |. Parser.symbol ")"
        , backslashEscape
        , Parser.map Regex.Literal plainLiteral
        ]
  in
  Parser.succeed (\atom f -> f atom)
    |= withoutRepetition
    |= maybeRepeat

maybeRepeat : Parser (Regex.Atom -> Regex.Atom)
maybeRepeat =
  Parser.oneOf
    [ Parser.succeed (Regex.Repeat { min = 0, max = Nothing })
        |. Parser.symbol "*"
    , Parser.succeed (Regex.Repeat { min = 1, max = Nothing })
        |. Parser.symbol "+"
    , Parser.succeed identity
    ]

characterClass : Parser { negated : Bool, atoms : List Regex.CharClassAtom }
characterClass =
  let
    item =
      Parser.succeed (\c1 mc2 ->
        case mc2 of
          Nothing -> Regex.CCLiteral c1
          Just c2 -> Regex.CCRange c1 c2)
        |= oneChar (\c -> c /= ']')
        |= Parser.oneOf
            [ Parser.succeed Just
                |. Parser.symbol "-"
                |= oneChar (\_ -> True)
            , Parser.succeed Nothing
            ]
  in
  Parser.sequence
    { start = "["
    , separator = ""
    , end = "]"
    , spaces = Parser.succeed ()
    , item = item
    , trailing = Parser.Forbidden
    }
  |> Parser.map (\atoms ->
      case atoms of
        (Regex.CCLiteral '^' :: ((_ :: _) as rest)) -> { negated = True, atoms = rest }
        _ -> { negated = False, atoms = atoms }
    )

oneChar : (Char -> Bool) -> Parser Char
oneChar p =
  Parser.chompIf p
  |> Parser.getChompedString
  |> Parser.andThen (\s ->
        case String.uncons s of
          Nothing -> Parser.problem "no input"
          Just (c, _) -> Parser.succeed c)

reservedChars : Set Char
reservedChars =
  Set.fromList [ '|', '(', ')', '[', ']' ]

plainLiteral : Parser Char
plainLiteral = oneChar (\c -> not (Set.member c reservedChars))

backslashEscape : Parser Regex.Atom
backslashEscape =
  let
    interpret c =
      case c of
        'n' -> Regex.Literal '\n'
        'r' -> Regex.Literal '\r'
        _ -> Regex.Literal c
  in
  Parser.succeed interpret
    |. Parser.symbol "\\"
    |= oneChar (\_ -> True)

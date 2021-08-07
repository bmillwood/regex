module Regex exposing (..)

import Parser exposing (Parser, (|.), (|=))
import Set exposing (Set)

type alias Regex = List (List RegexAtom)

type RegexAtom
  = Literal Char
  | CharacterClass { negated : Bool, atoms : List CharClassAtom }
  | Capture Regex
  | Repeat RegexAtom Repetition

type CharClassAtom
  = CCLiteral Char
  | CCRange Char Char

type alias Repetition =
  { min : Maybe Int
  , max : Maybe Int
  }

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

atomParser : Parser RegexAtom
atomParser =
  Parser.lazy (\ () ->
    Parser.oneOf
      [ Parser.map CharacterClass characterClass
      , Parser.succeed Capture
          |. Parser.symbol "("
          |= parser
          |. Parser.symbol ")"
      , backslashEscape
      , Parser.map Literal plainLiteral
      ])

characterClass : Parser { negated : Bool, atoms : List CharClassAtom }
characterClass =
  let
    item =
      Parser.succeed (\c1 mc2 ->
        case mc2 of
          Nothing -> CCLiteral c1
          Just c2 -> CCRange c1 c2)
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
        (CCLiteral '^' :: ((_ :: _) as rest)) -> { negated = True, atoms = rest }
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

backslashEscape : Parser RegexAtom
backslashEscape =
  let
    interpret c =
      case c of
        'n' -> Literal '\n'
        'r' -> Literal '\r'
        _ -> Literal c
  in
  Parser.succeed interpret
    |. Parser.symbol "\\"
    |= oneChar (\_ -> True)

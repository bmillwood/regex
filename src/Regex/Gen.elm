module Regex.Gen exposing (..)

import Random

import Regex exposing (Regex)

regex : { size : Int } -> Random.Generator Regex
regex { size } =
  partition { size = size }
  |> Random.andThen (\sizes ->
        randomMap (\subSize -> atoms { size = subSize }) sizes
      )

atoms : { size : Int } -> Random.Generator (List Regex.Atom)
atoms { size } =
  partition { size = size }
  |> Random.andThen (\sizes ->
        randomMap (\subSize -> atom { size = subSize }) sizes
      )
  |> Random.andThen shuffle

atom : { size : Int } -> Random.Generator Regex.Atom
atom { size } =
  if size <= 1
  then Random.uniform Regex.StartOfInput [ Regex.EndOfInput ]
  else
    Random.uniform
      ( Random.map Regex.CharMatching (charMatch { size = size - 1 }) )
      [ Random.map
          Regex.Capture
          (Random.lazy (\ () -> regex { size = size - 1 }))
      , Random.map2
          Regex.Repeat
          repetition
          (Random.lazy (\ () -> atom { size = size - 1 }))
      ]
    |> Random.andThen identity

charMatch : { size : Int } -> Random.Generator Regex.CharMatch
charMatch { size } =
  if size <= 1
  then
    Random.uniform
      ( Random.map Regex.MatchLit weightedChar )
      [ Random.constant Regex.MatchAny ]
    |> Random.andThen identity
  else
    let
      classAtom =
        Random.uniform
          ( Random.map Regex.ClassLit weightedChar )
          [ Random.map3
              classRange
              (Random.weighted (0.1, True) [(0.9, False)])
              weightedChar
              weightedChar
          ]
        |> Random.andThen identity
      classRange flipped char1 char2 =
        if flipped == (char1 <= char2)
        then Regex.ClassRange char2 char1
        else Regex.ClassRange char1 char2
    in
    Random.map2
      (\n a -> Regex.MatchClass { negated = n, matchAtoms = a })
      (Random.uniform True [ False ])
      (Random.list (size - 1) classAtom)

weightedChar : Random.Generator Char
weightedChar =
  Random.weighted
      ( 0.9, (Random.int 0x20 0x7e) )
    [ ( 0.1, (Random.int 0x00 0x10FFFF) ) ]
  |> Random.andThen (\g -> Random.map Char.fromCode g)

repetition : Random.Generator Regex.Repetition
repetition =
  let
    range flipped min max =
      case (min, max) of
        (Just m, Just n) ->
          if flipped == (m <= n)
          then Regex.Range { min = Just n, max = Just m }
          else Regex.Range { min = Just m, max = Just n }
        _ -> Regex.Range { min = min, max = max }

  in
  Random.uniform
    ( Random.constant Regex.Optional )
    [ Random.constant Regex.ZeroOrMore
    , Random.constant Regex.OneOrMore
    , Random.map Regex.Exactly (Random.int 0 5)
    , Random.map3
        range
        (Random.weighted (0.2, True) [(0.8, False)])
        (maybe { pJust = 0.8 } (Random.int 0 5))
        (maybe { pJust = 0.8 } (Random.int 0 5))
    ]
  |> Random.andThen identity

partition : { size : Int } -> Random.Generator (List Int)
partition { size } =
  Random.int 1 size
  |> Random.andThen (\first ->
        if first == size
        then Random.constant [ size ]
        else
          Random.map
            (\p -> first :: p)
            (Random.lazy (\ () -> partition { size = size - first }))
      )
  |> Random.andThen shuffle

shuffle : List a -> Random.Generator (List a)
shuffle items =
  let
    shuffleLen n xs =
      case xs of
        [] -> Random.constant []
        y :: ys ->
          Random.map2
            (insertAt y)
            (Random.int 0 (n-1))
            (shuffleLen (n-1) ys)
    insertAt y i xs =
      if i == 0
      then y :: xs
      else
        case xs of
          [] -> [ y ]
          z :: zs -> z :: insertAt y (i-1) zs
  in
  shuffleLen (List.length items) items

maybe
  :  { pJust : Float }
  -> Random.Generator a
  -> Random.Generator (Maybe a)
maybe { pJust } gen =
  Random.weighted
      ( pJust, Random.map Just gen )
    [ ( 1 - pJust, Random.constant Nothing ) ]
  |> Random.andThen identity

randomMap
  :  (a -> Random.Generator b)
  -> List a
  -> Random.Generator (List b)
randomMap f =
  List.foldr
    (\x xs -> Random.map2 (::) (f x) xs)
    (Random.constant [])

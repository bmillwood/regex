module Variant.Html exposing (..)

import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events

import Variant exposing (Variant)

type alias Select a = List (Option a)

type alias Option a =
  { name : String
  , init : Maybe a -> a
  , selected : a -> Maybe (List (Html a))
  }

isSelected : Option a -> a -> Bool
isSelected option value =
  case option.selected value of
    Just _ -> True
    Nothing -> False

subOption : Variant a b -> Option b -> Option a
subOption variant option =
  { name = option.name
  , init =
      Maybe.andThen (Variant.match variant)
      >> option.init
      >> Variant.make variant
  , selected = \a ->
      Variant.match variant a
      |> Maybe.andThen option.selected
      |> Maybe.map (List.map (Html.map (Variant.make variant)))
  }

toHtml : Select a -> a -> List (Html a)
toHtml options value =
  let
    mkOption option =
      Html.option
        [ Attributes.selected (isSelected option value) ]
        [ Html.text option.name ]
    optionParams option =
      case option.selected value of
        Just params -> params
        Nothing -> []
  in
  Html.select
    [ Events.onInput (\s ->
        case List.filter (\opt -> opt.name == s) options of
          opt :: _ -> opt.init (Just value)
          _ -> value)
    ]
    (List.map mkOption options)
  :: List.concatMap optionParams options

ofUnit : String -> Variant a () -> Option a
ofUnit name variant =
  { name = name
  , init = always (Variant.make variant ())
  , selected =
      Variant.match variant
      >> Maybe.map (\ () -> [])
  }

ofVariant
  :  String
  -> Variant a b
  -> (Maybe a -> b)
  -> (b -> List (Html b))
  -> Option a
ofVariant name variant init params =
  { name = name
  , init = init >> Variant.make variant
  , selected =
      Variant.match variant
      >> Maybe.map (params >> List.map (Html.map (Variant.make variant)))
  }

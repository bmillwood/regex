module Variant.Html exposing (..)

import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events

import Variant exposing (Variant)

type alias Select a = List (Option a)

type alias Option a =
  { name : String
  , init : a -> a
  , selected : a -> Maybe (List (Html a))
  }

isSelected : Option a -> a -> Bool
isSelected option value =
  case option.selected value of
    Just _ -> True
    Nothing -> False

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
          opt :: _ -> opt.init value
          _ -> value)
    ]
    (List.map mkOption options)
  :: List.concatMap optionParams options

ofUnit : String -> Variant a () -> Option a
ofUnit name variant =
  { name = name
  , init = always (Variant.make variant ())
  , selected = \a ->
      Variant.match variant a
      |> Maybe.map (\ () -> [])
  }

ofVariant
  :  String
  -> Variant a b
  -> (a -> b)
  -> (b -> List (Html b))
  -> Option a
ofVariant name variant init params =
  { name = name
  , init = \a -> Variant.make variant (init a)
  , selected = \a ->
      Variant.match variant a
      |> Maybe.map (params >> List.map (Html.map (Variant.make variant)))
  }

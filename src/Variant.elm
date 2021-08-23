module Variant exposing (..)

type alias Variant a b =
  { match : a -> Maybe b
  , make : b -> a
  }

match : Variant a b -> a -> Maybe b
match v = v.match

make : Variant a b -> b -> a
make v = v.make

unit : a -> Variant a ()
unit a =
  { match = \b -> if a == b then Just () else Nothing
  , make = \() -> a
  }

compose : Variant a b -> Variant b c -> Variant a c
compose outer inner =
  { match = \a -> outer.match a |> Maybe.andThen inner.match
  , make = \c -> outer.make (inner.make c)
  }

is : Variant a b -> a -> Bool
is variant value =
  case variant.match value of
    Just _ -> True
    Nothing -> False

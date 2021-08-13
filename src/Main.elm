module Main exposing (main)

import Browser
import Html exposing (Html)
import Json.Decode

import Real
import Test

type Model
  = RealModel Real.Model
  | TestModel Test.Model

type Msg
  = RealMsg Real.Msg
  | TestMsg Test.Msg

init : Json.Decode.Value -> (Model, Cmd Msg)
init flags =
  let
    testMode =
      Json.Decode.at [ "location", "href" ] Json.Decode.string
      |> Json.Decode.map testFromHref

    testFromHref href =
      List.member "test=true" (String.split "?" href)
  in
  case Json.Decode.decodeValue testMode flags of
    Ok True ->
      case Test.init of
        (model, cmd) -> (TestModel model, Cmd.map TestMsg cmd)
    _ ->
      case Real.init of
        (model, cmd) -> (RealModel model, Cmd.map RealMsg cmd)

view : Model -> Html Msg
view model =
  case model of
    RealModel real -> Html.map RealMsg (Real.view real)
    TestModel test -> Html.map TestMsg (Test.view test)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case (msg, model) of
    (RealMsg rmsg, RealModel rmodel) ->
      case Real.update rmsg rmodel of
        (newModel, cmd) -> (RealModel newModel, Cmd.map RealMsg cmd)
    (TestMsg tmsg, TestModel tmodel) ->
      case Test.update tmsg tmodel of
        (newModel, cmd) -> (TestModel newModel, Cmd.map TestMsg cmd)
    _ -> (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

main =
  Browser.element
    { init          = init
    , view          = view
    , update        = update
    , subscriptions = subscriptions
    }

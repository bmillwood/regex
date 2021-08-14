module Main exposing (main)

import Browser
import Html exposing (Html)
import Json.Decode
import Url
import Url.Parser exposing ((<?>))
import Url.Parser.Query

import Real
import Test

type Model
  = RealModel Real.Model
  | TestModel Test.Model

type Msg
  = RealMsg Real.Msg
  | TestMsg Test.Msg

type Flags
  = TestMode
  | RealMode { initialInput : String }

init : Json.Decode.Value -> (Model, Cmd Msg)
init rawFlags =
  let
    flags =
      Json.Decode.decodeValue
        (Json.Decode.at [ "location", "href" ] Json.Decode.string)
        rawFlags
      |> Result.mapError Json.Decode.errorToString
      |> Result.map workaroundForProtocol
      |> Result.andThen (\href ->
           Url.fromString href
           |> Result.fromMaybe ("Url parsing failed on: " ++ href))
      |> Result.map workaroundForCannotIgnorePath
      |> Result.andThen (\url ->
           (Url.Parser.parse (Url.Parser.top <?> queryParser) url)
           |> Result.fromMaybe ("Query parser failed on url: " ++ Url.toString url))

    workaroundForProtocol url =
      -- The URL library can't parse file: URLs, but I want to.
      -- https://github.com/elm/url/issues/10
      case String.indices ":" url of
        [] -> url
        first :: _ ->
          "http" ++ String.dropLeft first url

    -- https://github.com/elm/url/issues/17
    workaroundForCannotIgnorePath url = { url | path = "" }

    real realFlags =
      case Real.init realFlags of
        (model, cmd) -> (RealModel model, Cmd.map RealMsg cmd)
  in
  case flags of
    Ok TestMode ->
      case Test.init of
        (model, cmd) -> (TestModel model, Cmd.map TestMsg cmd)
    Ok (RealMode { initialInput }) ->
      real { initialInput = initialInput }
    Err err ->
      real { initialInput = "" }

queryParser : Url.Parser.Query.Parser Flags
queryParser =
  let
    make testV initV =
      case testV of
        Just "true" -> TestMode
        _ -> RealMode { initialInput = Maybe.withDefault "" initV }
  in
  Url.Parser.Query.map2 make
    (Url.Parser.Query.string "test")
    (Url.Parser.Query.string "init")

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

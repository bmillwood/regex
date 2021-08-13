module Test exposing (Model, Msg, init, view, update)

import Html exposing (Html)

type Model = TestModel
type Msg = TestMsg

init : (Model, Cmd Msg)
init = (TestModel, Cmd.none)

view : Model -> Html Msg
view TestModel = Html.text "No tests yet!"

update : Msg -> Model -> (Model, Cmd Msg)
update TestMsg TestModel = (TestModel, Cmd.none)

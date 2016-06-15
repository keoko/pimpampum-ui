import Html exposing (Html, span, text, div, button)
import Html.App as Html
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import String exposing (reverse)

main = Html.beginnerProgram { model = "Hello World", view = view, update = update }

type Msg = Reverse

update msg model =
    case msg of
        Reverse -> String.reverse model


view model =
    div []
        [button [onClick Reverse] [text "reverse"]
        ,text model]

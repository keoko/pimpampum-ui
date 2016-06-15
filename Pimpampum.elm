import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import String exposing (reverse)

main = Html.beginnerProgram { model = initialModel, view = view, update = update }

type Msg = Reverse

type alias Model =
    { name : String }

initialModel : Model
initialModel = {name = "test"}

update msg model =
    case msg of
        Reverse -> {name = String.reverse model.name}


view model =
    div []
        [h1 [] [text "pimpampum-ui"]
        , div [] [text model.name]
        , button [onClick Reverse] [text "reverse"]]

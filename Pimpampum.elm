import Html exposing (Html, span, text)
import Html.Attributes exposing (class, style)

main = span [class "welcome-message", style [("color", "blue")]] [text "Hello, World!"]

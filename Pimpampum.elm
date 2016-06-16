import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

main = Html.beginnerProgram { model = initialModel, view = view, update = update }

-- Model
type Msg = Delete Int

type alias Model =
    { items : List Item }

type alias Item =
    { id : Int
    , sku : String
    , name : String
    , description : String
    }

initialModel : Model
initialModel = {
    items = [
         { id = 1
         , sku = "sku1"
         , name = "test1"
         , description = "description1"
         }
        ,{ id = 2
         , sku = "sku2"
         , name = "test1"
         , description = "description1"
         }
        ]}

-- Update
update msg model = 
    case msg of
        Delete id ->
            { model | items = List.filter (\i -> i.id /= id) model.items }


-- Views
itemListView items = ul [id "item-list"] (List.map (itemView) items)

itemView item =
    li []
        [div [] [text "id:", text (toString item.id)]
        ,div [] [text "name:", text item.name]
        ,div [] [text "sku:", text item.sku]
        ,div [] [text "description:", text item.description]
        ,button [onClick (Delete item.id)] [text "delete"]
        ]


view model =
    div []
        [h1 [] [text "pimpampum-ui"]
        , itemListView model.items
        ]

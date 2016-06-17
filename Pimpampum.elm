import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Debug exposing (log)

main = Html.beginnerProgram { model = initialModel, view = view, update = update }

-- Model
type Msg = Delete Int | Add | Sku String | Name String | Description String

type alias Model =
    { items : List Item
    , uid : Int
    , skuField : String
    , nameField : String
    , descriptionField : String
    }

type alias Item =
    { id : Int
    , sku : String
    , name : String
    , description : String
    }

initialModel : Model
initialModel = { uid = 1
               , skuField = ""
               , nameField = ""
               , descriptionField = ""
               , items = [
                      { id = 1
                      , sku = "sku1"
                      , name = "test1"
                      , description = "description1"
                      }
                     , { id = 2
                       , sku = "sku2"
                       , name = "test1"
                           , description = "description1"
                       }
                     ]
               }

newItem uid sku name description =
    { id = uid
    , sku = sku
    , name = name
    , description = description
    }

-- Update
update msg model = 
    case msg of
        Delete id ->
            { model | items = List.filter (\i -> i.id /= id) model.items }
        Add ->
            { model |
            uid = model.uid + 1
            , skuField = ""
            , nameField = ""
            , descriptionField = ""
            , items = model.items
                      ++ [ newItem
                           model.uid
                           model.skuField
                           model.nameField
                           model.descriptionField
                         ]
            }
        Sku sku ->
            { model | skuField = sku }
        Name name ->
            { model | nameField = name }
        Description description ->
            { model | descriptionField = description }

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

itemFormView model =
    div []
        [text "sku:"
        ,input [onInput Sku, value model.skuField] []
        ,text "name:"
        ,input [onInput Name, value model.nameField] []
        ,text "description:"
        ,input [onInput Description, value model.descriptionField] []
        ,button [onClick Add] [text "add"]
        ]

view model =
    div []
        [h1 [] [text "pimpampum-ui"]
        , itemListView model.items
        , itemFormView model
        ]

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (id, value)
import Html.Events exposing (onClick, onInput)
import Debug exposing (log)
import Json.Encode as Json
import Json.Decode exposing (Decoder, at, decodeString, decodeValue, succeed, int, string, object1, object4, list, (:=))
import Http
import Task


url = "http://localhost:4000/api/items"

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- Model
type EditMode = AddMode | UpdateMode

type Msg = Edit Int
         | Delete Int
         | Update Int
         | Reset
         | Add
         | Sku String
         | Name String
         | Description String
         | FetchFail Http.Error
         | FetchSucceed Items

type alias Model =
    { items : List Item
    , uid : Int
    , idField : Int
    , skuField : String
    , nameField : String
    , descriptionField : String
    , editMode : EditMode
    }

type alias Item =
    { id : Int
    , sku : String
    , name : String
    , description : String
    }

type alias Items = List Item

initialModel : Model
initialModel = { uid = 1
               , idField = 0
               , skuField = ""
               , nameField = ""
               , descriptionField = ""
               , editMode = AddMode
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

init : (Model, Cmd Msg)
init =
    (initialModel
    , getItems
    )


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
            ({ model | items = List.filter (\i -> i.id /= id) model.items }, Cmd.none)
        Edit id ->
            let
                items = List.filter (\i -> i.id == id) model.items
            in
                case List.head(items) of
                    Just item -> ({ model |
                                   editMode = UpdateMode
                                 , idField = item.id
                                 , skuField = item.sku
                                 , nameField = item.name
                                 , descriptionField = item.description
                                 }, Cmd.none)
                    Nothing -> (model, Cmd.none)
        Add ->
            ({ model |
            uid = model.uid + 1
            , idField = 0
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
            }, Cmd.none)
        Update id ->
            let
                updateItem i =
                    if i.id == id then
                        { i |
                          sku = model.skuField
                        , name = model.nameField
                        , description = model.descriptionField
                        }
                    else
                        i
            in
                ({ model | items = List.map updateItem model.items }, Cmd.none)
        Sku sku ->
            ({ model | skuField = sku }, Cmd.none)
        Name name ->
            ({ model | nameField = name }, Cmd.none)
        Description description ->
            ({ model | descriptionField = description }, Cmd.none)
        Reset ->
            ({ model |
              editMode = AddMode
            , idField = 0
            , skuField = ""
            , nameField = ""
            , descriptionField = ""
            }, Cmd.none)
        FetchFail _ ->
            -- todo: log the error in console
            (model, Cmd.none)
        FetchSucceed items ->
            ({ model | items = items }, Cmd.none)

-- Views
itemListView items = ul [id "item-list"] (List.map (itemView) items)

itemView item =
    li []
        [div [] [text "id:", text (toString item.id)]
        ,div [] [text "name:", text item.name]
        ,div [] [text "sku:", text item.sku]
        ,div [] [text "description:", text item.description]
        ,button [onClick (Delete item.id)] [text "delete"]
        ,button [onClick (Edit item.id)] [text "edit"]
        ]

itemFormView model =
    div []
        [ text "sku:"
        , input [onInput Sku, value model.skuField] []
        , text "name:"
        , input [onInput Name, value model.nameField] []
        , text "description:"
        , input [onInput Description, value model.descriptionField] []
        , editButton model
        , resetButton
        ]

editButton model =
    case model.editMode of
        AddMode -> button [onClick Add] [text "add"]
        UpdateMode -> button [onClick (Update model.idField)] [text "update"]

resetButton =
    button [onClick Reset] [text "reset"]

view model =
    div []
        [h1 [] [text "pimpampum-ui"]
        , itemListView model.items
        , itemFormView model
        ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- HTTP
itemDecoder : Decoder Item
itemDecoder =
    object4 Item
        ( "id" := int )
        ( "sku" := string )
        ( "name" := string )
        ( "description" := string )

itemsDecoder =
    at ["data"] (list itemDecoder)

-- jsonString = "{\"items\": [{\"name\": \"this is a test111\", \"description\": \"desc\"},{\"name\": \"this is a test222\", \"description\": \"desc2\"}]}"
jsonString = "[{\"name\": \"this is a test111\", \"description\": \"desc\"},{\"name\": \"this is a test222\", \"description\": \"desc2\"}]"

getItems : Cmd Msg
getItems =
    Task.perform FetchFail FetchSucceed (Http.get itemsDecoder url)

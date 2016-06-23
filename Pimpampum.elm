import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (id, value)
import Html.Events exposing (onClick, onInput)
import Debug exposing (log)
import Json.Encode as Json
import Json.Decode exposing (Decoder, at, decodeString, decodeValue, succeed, int, string, object1, object4, list, (:=))
import Http
import Task
import Dict

host = "http://localhost:4000"

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
         | DeleteItemFail Http.RawError
         | DeleteItemSucceed Res1
         | AddItemFail Http.RawError
         | AddItemSucceed Res1

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


-- todo: find the type name
type alias Res1 =
    { headers : Dict.Dict String String
    , status : Int
    , statusText : String
    , url : String
    , value : Http.Value
    }

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
            ({ model | items = List.filter (\i -> i.id /= id) model.items }, (deleteItem id))
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
            -- todo: use item id generated by the backend, not the uid + 1
            let
                item' = newItem model.uid model.skuField model.nameField model.descriptionField
            in
                ({ model |
                       uid = model.uid + 1
                 , idField = 0
                 , skuField = ""
                 , nameField = ""
                 , descriptionField = ""
                 , items = item' :: model.items
            }, (addItem item'))
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
        DeleteItemFail _ ->
            (model, Cmd.none)
        DeleteItemSucceed _ ->
            (model, Cmd.none)
        AddItemFail _ ->
            (model, Cmd.none)
        AddItemSucceed _ ->
            (model, Cmd.none)

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


getItems : Cmd Msg
getItems =
    let
        url = host ++ "/api/items"
    in
        Task.perform FetchFail FetchSucceed (Http.get itemsDecoder url)

deleteItem id =
    let
        url = host ++ "/api/items/" ++ (toString id)

        request = { verb = "DELETE"
                  , headers = [("Content-Type", "application/json")]
                  , url = url
                  , body = Http.string ""
                  }

        sendRequest = Http.send Http.defaultSettings request
    in
        Task.perform DeleteItemFail DeleteItemSucceed sendRequest


addItem item =
    let
        url = host ++ "/api/items/"

        encodedItem = Json.object [ ("id", Json.int item.id)
                             , ("sku", Json.string item.sku)
                             , ("name", Json.string item.name)
                             , ("description", Json.string item.description)]
                      |> (\x -> Json.object [("item", x)])
                      |> Json.encode 0

        request = { verb = "POST"
                  , headers = [("Content-Type", "application/json")]
                  , url = url
                  , body = Http.string encodedItem
                  }

        sendRequest = Http.send Http.defaultSettings request
    in
        Task.perform AddItemFail AddItemSucceed sendRequest

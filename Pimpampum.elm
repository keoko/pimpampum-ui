import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (id, value)
import Html.Events exposing (onClick, onInput)
-- import Debug exposing (log)
import Json.Encode as Json
import Json.Decode exposing (Decoder, at, decodeString, decodeValue, succeed, int, string, object1, object4, list, (:=))
import Http
import Task
import Dict
import Phoenix.Socket
import Phoenix.Channel
import Phoenix.Push

host : String
host = "http://localhost:4000"

socketServer : String
socketServer = "ws://localhost:4000/socket/websocket"

channelName : String
channelName = "channel:item"

main : Program Never
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- Model
type alias ItemId = {
        id : String
    }

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
         | UpdateItemFail Http.RawError
         | UpdateItemSucceed Res1
         | PhoenixMsg (Phoenix.Socket.Msg Msg)
         | DeleteItemMessage Json.Value
         | AddItemMessage Json.Value
         | UpdateItemMessage Json.Value
         | JoinChannel

type alias Model =
    { items : List Item
    , uid : Int
    , idField : Int
    , skuField : String
    , nameField : String
    , descriptionField : String
    , editMode : EditMode
    , phxSocket : Phoenix.Socket.Socket Msg
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
               , phxSocket = initPhxSocket
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


newItem : Int -> String -> String -> String -> Item
newItem uid sku name description =
    { id = uid
    , sku = sku
    , name = name
    , description = description
    }

-- Update
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        JoinChannel ->
            let
                channel = Phoenix.Channel.init channelName
                ( phxSocket, phxCmd ) = Phoenix.Socket.join channel model.phxSocket
            in
                ( { model | phxSocket = phxSocket }
                , Cmd.map PhoenixMsg phxCmd
                )

        PhoenixMsg msg ->
            let
                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.update msg model.phxSocket
            in
                ( { model | phxSocket = phxSocket }
                , Cmd.map PhoenixMsg phxCmd
                )

        DeleteItemMessage raw ->
            case decodeValue deleteItemMessageDecoder raw of
                Ok {id} ->
                    ({ model | items = List.filter (\i -> (toString i.id) /= id) model.items }, Cmd.none)
                _ -> ({ model | skuField = "todo: handle deleteitemmessage error"} , Cmd.none )

        AddItemMessage raw ->
            case decodeValue itemDecoder raw of
                Ok item ->
                    let
                        item' = newItem item.id item.sku item.name item.description
                    in
                        ({ model | items = item' :: model.items }, Cmd.none )
                _ -> ({ model | skuField = "todo: handle additemmessage error"} , Cmd.none )

        UpdateItemMessage raw ->
            case decodeValue itemDecoder raw of
                Ok item ->
                    let
                        item' = newItem item.id item.sku item.name item.description
                    in
                        ({ model | items = (updateItemInList model.items item') }, Cmd.none )
                _ -> ({ model | skuField = "todo: handle updateitemmessage error"} , Cmd.none )

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
                item' = Item id model.skuField model.nameField model.descriptionField
            in
                ({ model | items = (updateItemInList model.items item')}, (updateItem item'))
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
        UpdateItemFail _ ->
            (model, Cmd.none)
        UpdateItemSucceed _ ->
            (model, Cmd.none)

-- Views
itemListView : Items -> Html Msg
itemListView items = ul [id "item-list"] (List.map (itemView) items)

itemView : Item -> Html Msg
itemView item =
    li []
        [div [] [text "id:", text (toString item.id)]
        ,div [] [text "name:", text item.name]
        ,div [] [text "sku:", text item.sku]
        ,div [] [text "description:", text item.description]
        ,button [onClick (Delete item.id)] [text "delete"]
        ,button [onClick (Edit item.id)] [text "edit"]
        ]

itemFormView : Model -> Html Msg
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

editButton : Model -> Html Msg
editButton model =
    case model.editMode of
        AddMode -> button [onClick Add] [text "add"]
        UpdateMode -> button [onClick (Update model.idField)] [text "update"]

resetButton : Html Msg
resetButton =
    button [onClick Reset] [text "reset"]

view : Model -> Html Msg
view model =
    div []
        [h1 [] [text "pimpampum-ui"]
        , button [ onClick JoinChannel ] [ text "Join lobby" ]
        , itemListView model.items
        , itemFormView model
        ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Phoenix.Socket.listen model.phxSocket PhoenixMsg


-- HTTP

itemDecoder : Decoder Item
itemDecoder =
    object4 Item
        ( "id" := int )
        ( "sku" := string )
        ( "name" := string )
        ( "description" := string )

itemsDecoder : Decoder Items
itemsDecoder =
    at ["data"] (list itemDecoder)


getItems : Cmd Msg
getItems =
    let
        url = host ++ "/api/items"
    in
        Task.perform FetchFail FetchSucceed (Http.get itemsDecoder url)

deleteItem : Int -> Cmd Msg
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


addItem : Item -> Cmd Msg
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


updateItem : Item -> Cmd Msg
updateItem item =
    let
        url = host ++ "/api/items/" ++ (toString item.id)

        encodedItem = Json.object [ ("id", Json.int item.id)
                             , ("sku", Json.string item.sku)
                             , ("name", Json.string item.name)
                             , ("description", Json.string item.description)]
                      |> (\x -> Json.object [("item", x)])
                      |> Json.encode 0

        request = { verb = "PUT"
                  , headers = [("Content-Type", "application/json")]
                  , url = url
                  , body = Http.string encodedItem
                  }

        sendRequest = Http.send Http.defaultSettings request
    in
        Task.perform UpdateItemFail UpdateItemSucceed sendRequest


initPhxSocket : Phoenix.Socket.Socket Msg
initPhxSocket =
    Phoenix.Socket.init socketServer
        |> Phoenix.Socket.withDebug
        |> Phoenix.Socket.on "item:delete" channelName DeleteItemMessage
        |> Phoenix.Socket.on "item:add" channelName AddItemMessage
        |> Phoenix.Socket.on "item:update" channelName UpdateItemMessage


deleteItemMessageDecoder : Decoder ItemId
deleteItemMessageDecoder =
    object1 ItemId ("id" := string)



updateItemInList items item =
    let
        -- todo use item' in updateItem local function
        update i =
            if i.id == item.id then
                { i |
                  sku = item.sku
                , name = item.name
                , description = item.description
                }
            else
                i
    in
        List.map update items

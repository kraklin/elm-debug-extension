port module Panel exposing (main)

import Browser
import Expandable exposing (ElmValue)
import Html exposing (Html)
import Html.Events as Events
import Html.Events.Extra as Events
import Json.Decode exposing (Value)
import Murmur3


port logReceived : (String -> msg) -> Sub msg


port parse : ( Int, String ) -> Cmd msg


port parsedReceived : (Value -> msg) -> Sub msg


type alias Flags =
    ()


type alias DebugMessage =
    { count : Int
    , tag : String
    , value : ElmValue
    , hash : Int
    }


type alias Model =
    { messages : List DebugMessage
    }


type Msg
    = LogReceived String
    | Clear
    | ParsedReceived Value
    | Toggle Expandable.Key


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { messages = [] }, Cmd.none )


messageHash : String -> Int
messageHash input =
    Murmur3.hashString 1234 input


incrementLastMessageCount : List DebugMessage -> List DebugMessage
incrementLastMessageCount messages =
    case messages of
        [] ->
            []

        lastMessage :: rest ->
            { lastMessage | count = lastMessage.count + 1 } :: rest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LogReceived log ->
            let
                lastHash =
                    List.head model.messages |> Maybe.map .hash

                hash =
                    messageHash log

                ( messages, cmd ) =
                    if lastHash == Just hash then
                        ( incrementLastMessageCount model.messages, Cmd.none )

                    else
                        ( model.messages, parse ( hash, log ) )
            in
            ( { model | messages = messages }, cmd )

        Clear ->
            ( { messages = []
              }
            , Cmd.none
            )

        ParsedReceived parsedValue ->
            let
                decodedValue =
                    Expandable.decodeParsedValue parsedValue

                messages =
                    case decodedValue of
                        Ok { hash, tag, value } ->
                            { count = 1, tag = tag, value = value, hash = hash } :: model.messages

                        Err e ->
                            model.messages
            in
            ( { model | messages = messages }, Cmd.none )

        Toggle path ->
            let
                updatedMessages =
                    case model.messages of
                        [] ->
                            []

                        m :: rest ->
                            { m | value = updatedElmValue m.value } :: rest

                updatedElmValue val =
                    Expandable.map path Expandable.toggle val
            in
            ( { model | messages = updatedMessages }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ logReceived LogReceived
        , parsedReceived ParsedReceived
        ]


view : Model -> Html Msg
view model =
    let
        messages =
            List.map
                (\{ tag, value, count } ->
                    Html.li
                        [ Events.onClickStopPropagation <| Toggle []
                        ]
                        [ Html.text <| "(" ++ String.fromInt count ++ ") " ++ tag
                        , Expandable.viewValue Toggle [] value
                        ]
                )
                model.messages
    in
    Html.div []
        [ Html.button [ Events.onClick Clear ] [ Html.text "Clear all" ]
        , Html.ul [] messages
        ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

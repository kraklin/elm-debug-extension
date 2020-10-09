port module Panel exposing (main)

import Browser
import Html exposing (Html)
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Extra as Decode
import Murmur3


port logReceived : (String -> msg) -> Sub msg


port parse : ( Int, String ) -> Cmd msg


port parsedReceived : (Value -> msg) -> Sub msg


type alias Flags =
    ()


type alias DebugMessage =
    { count : Int
    , message : String
    , hash : Int
    }


type alias Model =
    { messages : List DebugMessage
    }


type alias DecodedLog =
    { log : String, hash : Int }


type Msg
    = LogReceived String
    | Clear
    | ParsedReceived Value


debugDecoder : Decoder String
debugDecoder =
    Decode.field "name" Decode.string


logDecoder : Decoder DecodedLog
logDecoder =
    Decode.succeed DecodedLog
        |> Decode.andMap (Decode.field "log" debugDecoder)
        |> Decode.andMap (Decode.field "hash" Decode.int)


decodeParsedValue : Value -> Result Decode.Error DecodedLog
decodeParsedValue value =
    Decode.decodeValue logDecoder value


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { messages = [] }, Cmd.none )


messageHash : String -> Int
messageHash input =
    Murmur3.hashString 1234 input


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

        ParsedReceived value ->
            let
                decodedValue =
                    decodeParsedValue value

                messages =
                    case decodedValue of
                        Ok { hash, log } ->
                            { count = 1, message = log, hash = hash } :: model.messages

                        Err e ->
                            model.messages
            in
            ( { model | messages = messages }, Cmd.none )


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
            List.map (\{ message, count } -> Html.li [] [ Html.text <| "(" ++ String.fromInt count ++ ") " ++ message ]) model.messages
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

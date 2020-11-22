port module Panel exposing (main)

import Browser
import Css
import Dict
import Expandable exposing (ElmValue)
import Html.Events.Extra as Events
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Json.Decode as Decode exposing (Value)
import Json.Decode.Extra as Decode
import List.Extra as List
import Murmur3


port logReceived : (( String, String ) -> msg) -> Sub msg


port parse : ( Int, String ) -> Cmd msg


port parsedReceived : (Value -> msg) -> Sub msg


port bulkLogReceived : (Value -> msg) -> Sub msg


port bulkParse : List ( BulkMessage, Int ) -> Cmd msg


port bulkParsedReceived : (Value -> msg) -> Sub msg


type alias Flags =
    ()


type alias DebugMessage =
    { count : Int
    , tag : String
    , value : ElmValue
    , hash : Int
    , isoTimestamp : String
    }


type alias BulkMessage =
    { time : String
    , log : String
    , hash : Int
    }


type alias Model =
    { messages : List DebugMessage
    }


type Msg
    = LogReceived ( String, String )
    | BulkLogReceived Value
    | Clear
    | ParsedReceived Value
    | BulkParsedReceived Value
    | Toggle Int Expandable.Key


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { messages = [] }, Cmd.none )


messageHash : String -> Int
messageHash input =
    Murmur3.hashString 1234 input


updateLastMessage : String -> List DebugMessage -> List DebugMessage
updateLastMessage isoTimestamp messages =
    case messages of
        [] ->
            []

        lastMessage :: rest ->
            { lastMessage | count = lastMessage.count + 1, isoTimestamp = isoTimestamp } :: rest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LogReceived ( isoTime, log ) ->
            let
                lastHash =
                    List.head model.messages |> Maybe.map .hash

                hash =
                    messageHash log

                ( messages, cmd ) =
                    if lastHash == Just hash then
                        ( updateLastMessage isoTime model.messages, Cmd.none )

                    else
                        ( model.messages, parse ( hash, log ) )
            in
            ( { model | messages = messages }, cmd )

        BulkLogReceived messages ->
            let
                bulkMessageDecoder =
                    Decode.map2 (\t l -> BulkMessage t l (messageHash l))
                        (Decode.field "time" Decode.string)
                        (Decode.field "log" Decode.string)

                decodedValues : Result Decode.Error (List BulkMessage)
                decodedValues =
                    Decode.decodeValue
                        (Decode.list bulkMessageDecoder)
                        messages

                toParse =
                    case decodedValues of
                        Ok values ->
                            values
                                |> List.groupWhile (\v1 v2 -> v1.hash == v2.hash)
                                |> List.map (\( v, list ) -> ( v, List.length list + 1 ))
                                |> bulkParse

                        Err err ->
                            Cmd.none
            in
            ( model, toParse )

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
                        Ok { hash, tag, isoTimestamp, value } ->
                            { count = 1
                            , tag = tag
                            , value = value
                            , hash = hash
                            , isoTimestamp = isoTimestamp
                            }
                                :: model.messages

                        Err e ->
                            model.messages
            in
            ( { model | messages = messages }, Cmd.none )

        BulkParsedReceived parsedValue ->
            let
                decoder =
                    Decode.map2
                        (\log count ->
                            { count = count
                            , tag = log.tag
                            , value = log.value
                            , hash = log.hash
                            , isoTimestamp = log.isoTimestamp
                            }
                        )
                        Expandable.logDecoder
                        (Decode.field "count" Decode.int)

                decodedMessages =
                    case Decode.decodeValue (Decode.list decoder) parsedValue of
                        Ok messages ->
                            List.reverse messages ++ model.messages

                        Err _ ->
                            model.messages
            in
            ( { model | messages = decodedMessages }, Cmd.none )

        Toggle idx path ->
            let
                updatedMessages =
                    List.indexedMap
                        (\messageIndex message ->
                            if messageIndex == idx then
                                { message | value = updatedElmValue message.value }

                            else
                                message
                        )
                        model.messages

                updatedElmValue val =
                    Expandable.map path Expandable.toggle val
            in
            ( { model | messages = updatedMessages }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ logReceived LogReceived
        , bulkLogReceived BulkLogReceived
        , parsedReceived ParsedReceived
        , bulkParsedReceived BulkParsedReceived
        ]


view : Model -> Html Msg
view model =
    let
        messages =
            model.messages
                |> List.indexedMap
                    (\idx { tag, value, count, hash } ->
                        Html.div
                            [ Attrs.css
                                [ Css.border3 (Css.px 1) Css.solid (Css.hex "ff00ff")
                                , Css.marginBottom (Css.px 12)
                                , Css.backgroundColor (Css.hex "ffffff")
                                , Css.padding2 (Css.px 8) (Css.px 12)
                                ]
                            ]
                            [ Expandable.viewMessageHeader (Toggle idx) count tag value ]
                    )
                |> List.reverse
    in
    Html.styled Html.div
        [ Css.fontSize <| Css.px 12
        , Css.fontFamily <| Css.monospace
        , Css.backgroundColor <| Css.hex "f0f0f0"
        ]
        []
        [ Html.div
            [ Attrs.css
                [ Css.position Css.fixed
                , Css.displayFlex
                , Css.top (Css.px 0)
                , Css.height (Css.px 24)
                , Css.backgroundColor (Css.hex "ffffff")
                , Css.width (Css.pct 100)
                ]
            ]
            [ Html.button [ Events.onClick Clear ] [ Html.text "Clear all" ] ]
        , Html.div
            [ Attrs.css
                [ Css.paddingTop (Css.px 32)
                , Css.paddingLeft (Css.px 8)
                , Css.paddingRight (Css.px 8)
                ]
            ]
            messages
        ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view >> Html.toUnstyled
        , subscriptions = subscriptions
        }

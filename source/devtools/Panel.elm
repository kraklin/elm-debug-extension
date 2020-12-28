port module Panel exposing (Flags, Model, Msg(..), defaultFlags, init, main, update, view)

import Browser
import Css
import DebugMessages exposing (AddMessageData, DebugMessages)
import Expandable
import Html.Events.Extra as Events
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Json.Decode as Decode exposing (Value)
import Json.Decode.Extra as Decode
import List.Extra as List
import Task
import Theme exposing (Theme)


port logReceived : (( String, String ) -> msg) -> Sub msg


port bulkLogReceived : (Value -> msg) -> Sub msg


type alias Flags =
    { theme : Theme }


defaultFlags : Flags
defaultFlags =
    { theme = Theme.Light
    }


decodeFlags : Value -> Flags
decodeFlags jsonValue =
    let
        themeDecoder =
            Decode.string
                |> Decode.map
                    (\theme ->
                        case theme of
                            "dark" ->
                                Theme.Dark

                            _ ->
                                Theme.Light
                    )
    in
    jsonValue
        |> Decode.decodeValue
            (Decode.succeed Flags
                |> Decode.andMap (Decode.field "theme" themeDecoder)
            )
        |> Result.withDefault defaultFlags


type alias Model =
    { messages : DebugMessages
    , flags : Flags
    }


type Msg
    = LogReceived ( String, String )
    | BulkLogReceived Value
    | Clear
    | Toggle DebugMessages.Key Expandable.Key
    | ParsingError String


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { messages = DebugMessages.empty, flags = flags }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LogReceived ( isoTime, log ) ->
            case DebugMessages.add (AddMessageData isoTime log) model.messages of
                Ok msgs ->
                    ( { model | messages = msgs }, Cmd.none )

                Err err ->
                    ( model, Task.perform identity <| Task.succeed <| ParsingError err )

        ParsingError _ ->
            ( model, Cmd.none )

        BulkLogReceived bulkMessages ->
            let
                bulkMessageDecoder =
                    Decode.map2 AddMessageData
                        (Decode.field "time" Decode.string)
                        (Decode.field "log" Decode.string)

                decodedValues : Result Decode.Error (List AddMessageData)
                decodedValues =
                    Decode.decodeValue
                        (Decode.list bulkMessageDecoder)
                        bulkMessages

                messages : DebugMessages
                messages =
                    case decodedValues of
                        Ok values ->
                            DebugMessages.bulkAdd values model.messages

                        Err _ ->
                            model.messages
            in
            ( { model | messages = messages }, Cmd.none )

        Clear ->
            ( { model
                | messages = DebugMessages.empty
              }
            , Cmd.none
            )

        Toggle key path ->
            ( { model | messages = DebugMessages.toggleValue key path model.messages }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ logReceived LogReceived
        , bulkLogReceived BulkLogReceived
        ]


view : Model -> Html Msg
view model =
    let
        colors =
            Theme.themeColors model.flags.theme

        messages =
            DebugMessages.messages model.messages
                |> List.map
                    (\{ tag, value, count, key } ->
                        Html.div
                            [ Attrs.css
                                [ Css.marginBottom (Css.px 12)
                                , Css.backgroundColor colors.panelBackground
                                , Css.color colors.foreground
                                , Css.padding2 (Css.px 8) (Css.px 12)
                                ]
                            ]
                            [ Expandable.viewMessageHeader colors (Toggle key) count tag value ]
                    )
    in
    Html.styled Html.div
        [ Css.fontSize <| Css.px 12
        , Css.fontFamily <| Css.monospace

        --, Css.backgroundColor colors.backgroundColor
        , Css.flexGrow <| Css.int 1
        , Css.displayFlex
        , Css.flexDirection Css.column
        , Css.position Css.absolute
        , Css.top <| Css.px 0
        , Css.bottom <| Css.px 0
        , Css.left <| Css.px 0
        , Css.right <| Css.px 0
        ]
        []
        [ Html.div
            [ Attrs.css
                [ Css.displayFlex
                , Css.backgroundColor colors.background
                , Css.padding2 (Css.px 4) (Css.px 8)
                , Css.width (Css.pct 100)
                ]
            ]
            [ Html.button
                [ Events.onClick Clear
                , Attrs.css
                    [ Css.backgroundColor colors.buttonBackground
                    , Css.color colors.buttonForeground
                    , Css.padding2 (Css.px 4) (Css.px 8)
                    , Css.hover
                        [ Css.backgroundColor colors.primary
                        ]
                    ]
                ]
                [ Html.text "Clear all" ]
            ]
        , Html.div
            [ Attrs.css
                [ Css.padding <| Css.px 8
                , Css.displayFlex
                , Css.flexDirection Css.column
                , Css.overflow Css.auto
                ]
            ]
            messages
        ]


main : Program Value Model Msg
main =
    Browser.element
        { init = init << decodeFlags
        , update = update
        , view = view >> Html.toUnstyled
        , subscriptions = subscriptions
        }

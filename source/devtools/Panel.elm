port module Panel exposing (main)

import Browser
import Css
import DebugMessages exposing (AddMessageData, DebugMessages)
import DebugParser
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


port bulkLogReceived : (Value -> msg) -> Sub msg


type Theme
    = Light
    | Dark


type alias Flags =
    { theme : Theme }


defaultFlags : Flags
defaultFlags =
    { theme = Light
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
                                Dark

                            _ ->
                                Light
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


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { messages = DebugMessages.empty, flags = flags }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LogReceived ( isoTime, log ) ->
            ( { model | messages = DebugMessages.add (AddMessageData isoTime log) model.messages }, Cmd.none )

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


type alias ThemeColors =
    { background : Css.Color
    , foreground : Css.Color
    , buttonBackground : Css.Color
    , buttonForeground : Css.Color
    , primary : Css.Color
    , panelBackground : Css.Color
    , stringColor : Css.Color
    , internalsColor : Css.Color
    , keysColor : Css.Color
    , guidelinesColor : Css.Color
    , expandTriangleColor : Css.Color
    }


lightTheme : ThemeColors
lightTheme =
    { background = Css.hex "ffffff"
    , foreground = Css.hex "000000"
    , buttonBackground = Css.hex "f0f0f0"
    , buttonForeground = Css.hex "000000"
    , primary = Css.hex "ff00ff"
    , panelBackground = Css.rgba 0 0 0 0.03
    , stringColor = Css.hex "0000ff"
    , internalsColor = Css.hex "808080"
    , keysColor = Css.hex "ff00ff"
    , guidelinesColor = Css.hex "a0a0a0"
    , expandTriangleColor = Css.hex "808080"
    }


themeColors : Theme -> ThemeColors
themeColors theme =
    if theme == Dark then
        { lightTheme
            | background = Css.hex "0f0f0f"
            , foreground = Css.hex "F8F8F2"
            , buttonBackground = Css.hex "303030"
            , buttonForeground = Css.hex "F8F8F2"
            , panelBackground = Css.rgba 255 255 255 0.1
            , stringColor = Css.hex "E6DB74"
            , internalsColor = Css.hex "808080"
            , keysColor = Css.hex "F92672"
            , guidelinesColor = Css.hex "AE81FF"
            , expandTriangleColor = Css.hex "F8F8F0"
        }

    else
        lightTheme


view : Model -> Html Msg
view model =
    let
        colors =
            themeColors model.flags.theme

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
        , Css.maxHeight <| Css.vh 100
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

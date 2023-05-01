port module Panel exposing (Flags, Model, Msg(..), defaultFlags, init, main, update, view)

import Browser
import Browser.Dom as Dom
import Css
import DebugMessages exposing (AddMessageData, DebugMessages)
import Expandable
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Iso8601
import Json.Decode as Decode exposing (Value)
import Json.Decode.Extra as Decode
import Json.Encode as Encode
import Task
import Theme exposing (Theme)
import Time exposing (Month(..), Posix, Zone)


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
    , zone : Zone
    , filter : String
    , paused : Bool
    }


type Msg
    = NoOp
    | LogReceived ( String, String )
    | BulkLogReceived Value
    | Clear
    | Toggle DebugMessages.Key Expandable.Key
    | ParsingError String
    | GetZone Zone
    | FilterChanged String
    | TogglePaused
    | AutoscrollStopped
    | ShowMoreMessages


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { messages = DebugMessages.init
      , flags = flags
      , zone = Time.utc
      , filter = ""
      , paused = True
      }
    , Task.perform GetZone Time.here
    )


messagesDivId : String
messagesDivId =
    "debug-messages"


jumpToTheBottom : Cmd Msg
jumpToTheBottom =
    Dom.getViewportOf messagesDivId
        |> Task.andThen (\info -> Dom.setViewportOf messagesDivId 0 info.scene.height)
        |> Task.attempt (\_ -> NoOp)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        AutoscrollStopped ->
            ( { model
                | paused = False
                , messages = DebugMessages.setHoldOn model.messages
              }
            , Cmd.none
            )

        GetZone zone ->
            ( { model | zone = zone }, Cmd.none )

        FilterChanged filter ->
            ( { model | filter = filter }, Cmd.none )

        LogReceived ( isoTime, log ) ->
            let
                maybePosix =
                    Iso8601.toTime isoTime
                        |> Result.toMaybe

                triggerError =
                    Task.perform identity << Task.succeed << ParsingError
            in
            maybePosix
                |> Maybe.map
                    (\posix ->
                        case DebugMessages.add (AddMessageData posix log) model.messages of
                            Ok msgs ->
                                ( { model | messages = msgs }, Cmd.none )

                            Err err ->
                                ( model, triggerError err )
                    )
                |> Maybe.withDefault ( model, triggerError "time is not a valid ISO time format" )

        ParsingError _ ->
            ( model, Cmd.none )

        BulkLogReceived bulkMessages ->
            let
                timeDecoder =
                    Decode.andThen
                        (\isoTime ->
                            case Iso8601.toTime isoTime of
                                Ok posixTime ->
                                    Decode.succeed posixTime

                                Err _ ->
                                    Decode.fail <| "'" ++ isoTime ++ "' is invalid ISO Time format"
                        )
                        Decode.string

                bulkMessageDecoder =
                    Decode.map2 AddMessageData
                        (Decode.field "time" timeDecoder)
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
                | messages = DebugMessages.clear model.messages
              }
            , Cmd.none
            )

        Toggle key path ->
            ( { model
                | messages =
                    DebugMessages.toggleValue key path model.messages
                        |> DebugMessages.setHoldOn
                , paused = False
              }
            , Cmd.none
            )

        TogglePaused ->
            if model.paused then
                ( { model | paused = False, messages = DebugMessages.setHoldOn model.messages }, Cmd.none )

            else
                ( { model | paused = True, messages = DebugMessages.setHoldOff model.messages }, jumpToTheBottom )

        ShowMoreMessages ->
            ( { model | messages = DebugMessages.mergeHoldOnQueue model.messages }, jumpToTheBottom )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ logReceived LogReceived
        , bulkLogReceived BulkLogReceived
        ]


formattedTime : Zone -> Posix -> String
formattedTime zone posix =
    let
        monthToStr =
            case Time.toMonth zone posix of
                Jan ->
                    "01"

                Feb ->
                    "02"

                Mar ->
                    "03"

                Apr ->
                    "04"

                May ->
                    "05"

                Jun ->
                    "06"

                Jul ->
                    "07"

                Aug ->
                    "08"

                Sep ->
                    "09"

                Oct ->
                    "10"

                Nov ->
                    "11"

                Dec ->
                    "12"

        toTwoDigits num =
            num
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    --format time YYYY-MM-DD HH:MM:SS
    String.fromInt (Time.toYear zone posix)
        ++ "-"
        ++ monthToStr
        ++ "-"
        ++ toTwoDigits (Time.toDay zone posix)
        ++ " "
        ++ toTwoDigits (Time.toHour zone posix)
        ++ ":"
        ++ toTwoDigits (Time.toMinute zone posix)
        ++ ":"
        ++ toTwoDigits (Time.toSecond zone posix)
        ++ "."
        ++ String.fromInt (Time.toMillis zone posix)


buttonStyles : Theme.ThemeColors -> List Css.Style
buttonStyles colors =
    [ Css.backgroundColor colors.buttonBackground
    , Css.color colors.buttonForeground
    , Css.padding2 (Css.px 4) (Css.px 8)
    , Css.marginRight (Css.px 4)
    , Css.hover
        [ Css.backgroundColor colors.primary
        , Css.cursor Css.pointer
        ]
    ]


view : Model -> Html Msg
view model =
    let
        colors =
            Theme.themeColors model.flags.theme

        localTime time =
            formattedTime model.zone time

        messages =
            DebugMessages.messages model.messages
                |> List.filterMap
                    (\{ tag, value, count, key, time } ->
                        if String.isEmpty model.filter || String.contains (String.toLower model.filter) (String.toLower tag) then
                            Just <|
                                Html.div
                                    [ Attrs.css
                                        [ Css.backgroundColor colors.panelBackground
                                        , Css.color colors.foreground
                                        , Css.padding2 (Css.px 8) (Css.px 12)
                                        , Css.marginBottom (Css.px 8)
                                        , Css.boxShadow4 (Css.px 0) (Css.px 2) (Css.px 2) (Css.rgba 0 0 0 0.08)
                                        ]
                                    ]
                                    [ Expandable.viewMessageHeader colors (Toggle key) count tag (localTime time) value ]

                        else
                            Nothing
                    )
                |> List.reverse

        moreMessages =
            let
                moreMessagesCount =
                    DebugMessages.holdOnQueueSize model.messages
            in
            if DebugMessages.isHoldOn model.messages && moreMessagesCount > 0 then
                Html.div
                    [ Attrs.css
                        [ Css.displayFlex
                        , Css.justifyContent Css.spaceBetween
                        , Css.backgroundColor colors.panelBackground
                        , Css.color colors.foreground
                        , Css.padding2 (Css.px 8) (Css.px 12)
                        , Css.marginBottom (Css.px 8)
                        , Css.boxShadow4 (Css.px 0) (Css.px 2) (Css.px 2) (Css.rgba 0 0 0 0.08)
                        ]
                    ]
                    [ Html.text <| String.fromInt moreMessagesCount ++ " more messages"
                    , Html.button
                        [ Attrs.css <| buttonStyles colors
                        , Events.onClick ShowMoreMessages
                        ]
                        [ Html.text "Show More Messages"
                        ]
                    ]

            else
                Html.text ""
    in
    Html.styled Html.div
        [ Css.fontSize <| Css.px 12
        , Css.fontFamily <| Css.monospace
        , Css.backgroundColor colors.background
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
                , Css.justifyContent Css.spaceBetween
                ]
            ]
            [ Html.div []
                [ Html.button
                    [ Attrs.css <| buttonStyles colors
                    , Events.onClick Clear
                    ]
                    [ Html.text "Clear all" ]
                , Html.button
                    [ Attrs.css <| buttonStyles colors
                    , Events.onClick TogglePaused
                    ]
                    [ if model.paused then
                        Html.text "Pause"

                      else
                        Html.text "Resume"
                    ]
                ]
            , Html.input
                [ Attrs.css [ Css.padding2 (Css.px 4) (Css.px 8) ]
                , Events.onInput FilterChanged
                , Attrs.value model.filter
                , Attrs.placeholder "Filter tags"
                ]
                []
            ]
        , Html.node "x-autoscroll-div"
            [ Attrs.id messagesDivId
            , Attrs.attribute "autoscroll" (Encode.encode 0 <| Encode.bool model.paused)
            , Events.on "x-autoscroll-stopped"
                (Decode.succeed AutoscrollStopped
                    |> Decode.andThen
                        (\msg ->
                            if model.paused then
                                Decode.succeed msg

                            else
                                Decode.fail "Autoscrolled already stopped"
                        )
                )
            , Attrs.css
                [ Css.padding <| Css.px 8
                , Css.displayFlex
                , Css.flexDirection Css.column
                , Css.overflow Css.auto
                ]
            ]
            messages
        , moreMessages
        ]


main : Program Value Model Msg
main =
    Browser.element
        { init = init << decodeFlags
        , update = update
        , view = view >> Html.toUnstyled
        , subscriptions = subscriptions
        }

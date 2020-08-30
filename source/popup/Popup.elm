port module Popup exposing (..)

import Browser
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Html.Events as Events



-- PORTS FROM JAVASCRIPT


port sendRequest : Request -> Cmd msg


port openOptionsPage : () -> Cmd msg


port receive : (DebugOptions -> msg) -> Sub msg


type alias Flags =
    { version : String
    }


type alias DebugOptions =
    { active : Bool
    }


type alias Model =
    { options : DebugOptions
    , flags : Flags
    }


type alias Request =
    { action : String }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { flags = flags, options = { active = False } }
    , sendRequest { action = "GET_STATUS" }
    )


type Msg
    = NoOp
    | UpdateOptions DebugOptions
    | OpenOptionsPage
    | ToggleDebug


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateOptions newOptions ->
            ( { model | options = newOptions }, Cmd.none )

        ToggleDebug ->
            ( model, sendRequest { action = "TOGGLE_ACTIVE" } )

        OpenOptionsPage ->
            ( model, openOptionsPage () )



-- VIEW


dark : Element.Color
dark =
    Element.rgb255 52 73 94


yellow : Element.Color
yellow =
    Element.rgb255 218 158 38


blue : Element.Color
blue =
    Element.rgb255 101 181 202


green : Element.Color
green =
    Element.rgb255 112 181 60


grey : Element.Color
grey =
    Element.rgb255 128 128 128


red : Element.Color
red =
    Element.rgb255 181 75 59


footer : String -> Element Msg
footer version =
    Element.row
        [ Element.width Element.fill
        , Font.size 10
        , Font.color grey
        ]
        [ Input.button [ Element.alignLeft ] { label = Element.text "Options", onPress = Just OpenOptionsPage }
        , Element.el [ Element.alignRight ] (Element.text <| "v" ++ version)
        ]


content : Model -> Element Msg
content model =
    Element.column [ Element.width Element.fill, Element.centerX, Element.spacing 16 ]
        [ activeElement model.options.active
        , footer model.flags.version
        ]


activeElement : Bool -> Element Msg
activeElement isActive =
    let
        button =
            if isActive then
                Input.button
                    [ Background.color <| red
                    , Font.color (Element.rgb255 255 255 255)
                    , Border.rounded 3
                    , Element.padding 8
                    , Element.alignRight
                    ]
                    { label = Element.text "Turn OFF", onPress = Just <| ToggleDebug }

            else
                Input.button
                    [ Background.color <| green
                    , Font.color (Element.rgb255 255 255 255)
                    , Border.rounded 3
                    , Element.padding 8
                    , Element.alignRight
                    ]
                    { label = Element.text "Turn ON", onPress = Just <| ToggleDebug }
    in
    Element.row [ Element.width Element.fill, Element.spacing 16 ]
        [ Element.paragraph [ Font.color dark ]
            [ Element.el [] (Element.text "Debug helper is ")
            , Element.el [ Font.bold ]
                (Element.text <|
                    if isActive then
                        "ON"

                    else
                        "OFF"
                )
            ]
        , button
        ]


view : Model -> Html Msg
view model =
    Element.layout
        [ Element.width <| Element.px 300
        , Element.padding 12
        , Font.size 14
        ]
        (content model)


subscriptions : Model -> Sub Msg
subscriptions model =
    receive UpdateOptions


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

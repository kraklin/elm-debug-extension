port module Popup exposing (..)

import Browser
import Css exposing (int, pct, px)
import Css.Transitions as Transitions
import Html exposing (Html)
import Html.Styled as HtmlStyled exposing (Html)
import Html.Styled.Attributes as Attrs exposing (css, src)
import Html.Styled.Events as Events
import Svg exposing (Svg)
import Svg.Attributes as SvgAttrs



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


dark : Css.Color
dark =
    Css.rgb 52 73 94


darkGreen : Css.Color
darkGreen =
    Css.hex "598637"


yellow : Css.Color
yellow =
    Css.rgb 218 158 38


blue : Css.Color
blue =
    Css.rgb 101 181 202


green : Css.Color
green =
    Css.rgb 112 181 60


white : Css.Color
white =
    Css.rgb 255 255 255


grey : Css.Color
grey =
    Css.rgb 128 128 128


red : Css.Color
red =
    Css.rgb 181 75 59


cogIcon : Html msg
cogIcon =
    HtmlStyled.fromUnstyled <|
        Html.map never <|
            Svg.svg [ SvgAttrs.width "16", SvgAttrs.height "17" ]
                [ Svg.path [ SvgAttrs.d "M15.796 10.543L14.36 9.7a6.713 6.713 0 000-2.406l1.436-.844a.417.417 0 00.185-.48 8.572 8.572 0 00-1.843-3.242.4.4 0 00-.499-.079l-1.435.844a6.334 6.334 0 00-2.05-1.203V.607a.41.41 0 00-.316-.401 8.332 8.332 0 00-3.68 0 .41.41 0 00-.316.4v1.687a6.534 6.534 0 00-2.05 1.203l-1.431-.843a.396.396 0 00-.499.079A8.52 8.52 0 00.019 5.974a.413.413 0 00.185.48l1.436.843a6.713 6.713 0 000 2.406l-1.436.843a.417.417 0 00-.185.48 8.572 8.572 0 001.843 3.242.4.4 0 00.499.08l1.435-.844a6.334 6.334 0 002.049 1.203v1.686c0 .192.131.36.317.401a8.332 8.332 0 003.68 0 .41.41 0 00.316-.4v-1.687a6.534 6.534 0 002.049-1.203l1.435.843c.166.096.371.065.5-.079a8.52 8.52 0 001.842-3.242.426.426 0 00-.188-.483zm-7.798.695c-1.486 0-2.696-1.23-2.696-2.741 0-1.512 1.21-2.742 2.696-2.742 1.486 0 2.696 1.23 2.696 2.742 0 1.511-1.21 2.741-2.696 2.741z", SvgAttrs.fill "currentColor" ] []
                ]


commentIcon : Html msg
commentIcon =
    HtmlStyled.fromUnstyled <|
        Html.map never <|
            Svg.svg [ SvgAttrs.viewBox "0 0 512 512" ]
                [ Svg.path [ SvgAttrs.d "M256 32C114.6 32 0 125.1 0 240c0 47.6 19.9 91.2 52.9 126.3C38 405.7 7 439.1 6.5 439.5c-6.6 7-8.4 17.2-4.6 26S14.4 480 24 480c61.5 0 110-25.7 139.1-46.3C192 442.8 223.2 448 256 448c141.4 0 256-93.1 256-208S397.4 32 256 32zm0 368c-26.7 0-53.1-4.1-78.4-12.1l-22.7-7.2-19.5 13.8c-14.3 10.1-33.9 21.4-57.5 29 7.3-12.1 14.4-25.7 19.9-40.2l10.6-28.1-20.6-21.8C69.7 314.1 48 282.2 48 240c0-88.2 93.3-160 208-160s208 71.8 208 160-93.3 160-208 160z", SvgAttrs.fill "currentColor" ] []
                ]


header : Html Msg
header =
    HtmlStyled.div
        [ css
            [ Css.displayFlex
            , Css.padding2 (px 12) (px 18)
            , Css.backgroundColor dark
            , Css.color white
            ]
        ]
        [ HtmlStyled.img
            [ css
                [ Css.width (px 24)
                , Css.height (px 24)
                ]
            , src "/assets/elm-logo.png"
            ]
            []
        , HtmlStyled.div
            [ css
                [ Css.textTransform Css.uppercase
                , Css.marginLeft (px 12)
                , Css.alignSelf Css.center
                ]
            ]
            [ HtmlStyled.text "Elm Debug Extension"
            ]
        ]


footer : String -> Html Msg
footer version =
    HtmlStyled.div [ css [ Css.displayFlex, Css.padding2 (px 12) (px 18) ] ]
        [ HtmlStyled.div
            [ css
                [ Css.flex (int 1)
                , Css.fontSize (px 9)
                , Css.color (Css.hex "5A6378")
                , Css.alignSelf Css.center
                ]
            ]
            [ HtmlStyled.text <| "v" ++ version ]
        , HtmlStyled.button
            [ css
                [ Css.padding (px 0)
                , Css.border (px 0)
                , Css.cursor Css.pointer
                , Css.color grey
                , Css.hover
                    [ Css.color dark
                    ]
                , Transitions.transition
                    [ Transitions.color 200
                    ]
                ]
            , Attrs.title "Settings"
            , Events.onClick OpenOptionsPage
            ]
            [ cogIcon
            ]
        ]


content : Model -> Html Msg
content model =
    HtmlStyled.div
        [ css
            [ Css.width (px 300)
            , Css.fontFamilies [ "IBM Plex Sans" ]
            ]
        ]
        [ header
        , activeElement model.options.active
        , footer model.flags.version
        ]


toggle : Bool -> Msg -> Html Msg
toggle isActive msg =
    let
        holder =
            HtmlStyled.div
                [ css
                    [ Css.width (px 18)
                    , Css.height (px 18)
                    , Css.borderRadius (px 9)
                    , Css.backgroundColor white
                    ]
                ]
                []

        textCss =
            Css.batch
                [ Css.flex (int 1)
                , Css.alignSelf Css.center
                , Css.fontSize (px 10)
                , Css.textTransform Css.uppercase
                , Css.fontWeight Css.bold
                ]

        backgroundCss =
            Css.batch
                [ Css.displayFlex
                , Css.width (px 48)
                , Css.height (px 24)
                , Css.padding (px 3)
                , Css.backgroundColor dark
                , Css.color white
                , Css.border (px 0)
                , Css.borderRadius (px 12)
                , Css.cursor Css.pointer
                , Css.focus
                    [ Css.border (px 0)
                    , Css.outline Css.none
                    ]
                , Transitions.transition
                    [ Transitions.backgroundColor 200
                    ]
                ]
    in
    if isActive then
        HtmlStyled.button
            [ css
                [ backgroundCss
                , Css.flexDirection Css.rowReverse
                , Css.backgroundColor darkGreen
                ]
            , Events.onClick msg
            ]
            [ holder
            , HtmlStyled.div [ css [ textCss ] ] [ HtmlStyled.text "On" ]
            ]

    else
        HtmlStyled.button
            [ css [ backgroundCss ]
            , Events.onClick msg
            ]
            [ holder
            , HtmlStyled.div [ css [ textCss ] ] [ HtmlStyled.text "Off" ]
            ]


activeElement : Bool -> Html Msg
activeElement isActive =
    let
        ( bgColor, fgColor ) =
            if isActive then
                ( green, white )

            else
                ( white, dark )
    in
    HtmlStyled.div
        [ css
            [ Css.displayFlex
            , Css.flexDirection Css.row
            , Css.backgroundColor bgColor
            , Css.color fgColor
            , Css.padding2 (px 12) (px 18)
            , Transitions.transition
                [ Transitions.backgroundColor 200
                ]
            ]
        ]
        [ HtmlStyled.p
            [ css
                [ Css.flex2 (int 1) (int 0)
                , Css.margin (px 0)
                , Css.alignSelf Css.center
                , Css.fontSize (px 12)
                , Css.fontWeight (int 600)
                , Css.textTransform Css.uppercase
                ]
            ]
            [ HtmlStyled.text "Transform debug output"
            ]
        , toggle isActive ToggleDebug
        ]


view : Model -> Html Msg
view model =
    content model


subscriptions : Model -> Sub Msg
subscriptions model =
    receive UpdateOptions


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view >> HtmlStyled.toUnstyled
        , subscriptions = subscriptions
        }

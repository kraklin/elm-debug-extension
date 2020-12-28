module Main exposing (main)

import Browser
import Css
import DebugParser
import Expandable
import Html.Events.Extra as Events
import Html.Extra as Html
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Iso8601
import Json.Decode exposing (Value)
import Panel exposing (Msg(..))
import Task
import Time exposing (Posix)



{- Styling stuff -}


tinyGap : Css.Px
tinyGap =
    Css.px 4


smallGap : Css.Px
smallGap =
    Css.px 8


bigGap : Css.Px
bigGap =
    Css.px 12


type alias Model =
    { input : String
    , panelModel : Panel.Model
    , lastError : Maybe String
    }


type Msg
    = InputChanged String
    | ParseButtonClicked
    | PanelMsg Panel.Msg
    | ParseInput Posix


init : Value -> ( Model, Cmd Msg )
init flags =
    let
        ( panelModel, panelCmd ) =
            Panel.init Panel.defaultFlags
    in
    ( { input = ""
      , panelModel = panelModel
      , lastError = Nothing
      }
    , Cmd.map PanelMsg panelCmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputChanged str ->
            ( { model | input = str }, Cmd.none )

        ParseButtonClicked ->
            ( model, Task.perform ParseInput Time.now )

        ParseInput posixTime ->
            ( model, Task.perform identity <| Task.succeed <| PanelMsg <| Panel.LogReceived ( Iso8601.fromTime posixTime, model.input ) )

        PanelMsg (ParsingError str) ->
            ( { model | lastError = Just str }, Cmd.none )

        PanelMsg pMsg ->
            let
                ( pModel, pCmd ) =
                    Panel.update pMsg model.panelModel
            in
            ( { model | panelModel = pModel, lastError = Nothing }
            , Cmd.map PanelMsg pCmd
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


viewError : Maybe String -> Html Msg
viewError lastError =
    case lastError of
        Just err ->
            Html.styled Html.div
                [ Css.color <| Css.hex "AB091E"
                , Css.fontSize <| Css.px 12
                , Css.maxWidth <| Css.px 300
                , Css.marginTop bigGap
                ]
                []
                [ Html.styled Html.div
                    [ Css.fontWeight Css.bolder
                    , Css.fontSize <| Css.px 14
                    ]
                    []
                    [ Html.text "There was a problem while trying to parse the above debug.log: " ]
                , Html.styled Html.span
                    [ Css.color <| Css.hex "CF1124"
                    ]
                    []
                    [ Html.text err ]
                ]

        Nothing ->
            Html.text ""


view : Model -> Browser.Document Msg
view model =
    { title = ""
    , body =
        [ Html.toUnstyled <|
            Html.styled Html.div
                [ Css.displayFlex
                , Css.boxSizing Css.borderBox
                , Css.flexDirection Css.column
                , Css.position Css.fixed
                , Css.top <| Css.px 0
                , Css.bottom <| Css.px 0
                , Css.left <| Css.px 0
                , Css.right <| Css.px 0
                ]
                []
                [ Html.styled Html.header
                    [ Css.padding bigGap
                    , Css.backgroundColor <| Css.hex "616e7c"
                    , Css.color <| Css.hex "F5F7FA"
                    , Css.displayFlex
                    , Css.justifyContent Css.spaceBetween
                    ]
                    []
                    [ Html.styled Html.h1
                        [ Css.margin <| Css.px 0
                        , Css.fontSize <| Css.px 20
                        , Css.textTransform Css.uppercase
                        ]
                        []
                        [ Html.text "Debug.log parser" ]
                    , Html.styled Html.a
                        [ Css.color <| Css.hex "f5f7fa"
                        ]
                        [ Attrs.href "https://github.com/kraklin/elm-debug-extension" ]
                        [ Html.text "Source" ]
                    ]
                , Html.styled Html.div
                    [ Css.displayFlex
                    , Css.flexWrap Css.wrap
                    , Css.height <| Css.pct 100
                    ]
                    []
                    [ Html.styled Html.div
                        [ Css.displayFlex
                        , Css.flexGrow <| Css.int 1
                        , Css.flexDirection Css.column
                        , Css.minWidth <| Css.px 300
                        , Css.padding smallGap
                        , Css.backgroundColor <| Css.hex "F5F7FA"
                        ]
                        []
                        [ Html.styled Html.textarea
                            [ Css.width <| Css.pct 100
                            , Css.height <| Css.pct 75
                            , Css.boxSizing Css.borderBox
                            , Css.resize Css.none
                            , Css.marginBottom smallGap
                            , Css.padding smallGap
                            ]
                            [ Events.onInput InputChanged
                            , Attrs.placeholder "Paste debug.log message here..."
                            ]
                            []
                        , Html.styled Html.button
                            [ Css.padding2 tinyGap smallGap
                            , Css.cursor Css.pointer
                            , Css.width <| Css.px 200
                            , Css.alignSelf Css.flexEnd
                            , Css.backgroundColor <| Css.hex "81DEFD"
                            , Css.color <| Css.hex "035388"
                            , Css.borderRadius <| Css.px 4
                            , Css.border3 (Css.px 1) Css.solid (Css.hex "035388")
                            , Css.hover
                                [ Css.backgroundColor <| Css.hex "40C3F7"
                                ]
                            ]
                            [ Events.onClick ParseButtonClicked ]
                            [ Html.text "Parse debug.log" ]
                        , viewError model.lastError
                        ]
                    , Html.styled Html.div
                        [ Css.flexGrow <| Css.int 2
                        , Css.minWidth <| Css.px 400
                        , Css.position Css.relative
                        ]
                        []
                        [ Html.map PanelMsg <| Panel.view model.panelModel
                        ]
                    ]
                ]
        ]
    }


main : Program Value Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

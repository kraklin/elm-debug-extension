port module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events as Events



-- PORTS FROM JAVASCRIPT


type alias Model =
    { active : Bool }


type alias Request =
    { action : String }


port sendRequest : Request -> Cmd msg


port receive : (Model -> msg) -> Sub msg


init : () -> ( Model, Cmd Msg )
init _ =
    ( { active = False }
    , sendRequest { action = "GET_STATUS" }
    )


type Msg
    = NoOp
    | NewState Model
    | ToggleDebug


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NewState newModel ->
            ( newModel, Cmd.none )

        ToggleDebug ->
            ( model, sendRequest { action = "TOGGLE_ACTIVE" } )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.text
            ("[Active]: "
                ++ (if model.active then
                        "ON"

                    else
                        "OFF"
                   )
            )
        , Html.button [ Events.onClick ToggleDebug ] [ Html.text "toggle" ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    receive NewState


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

port module Panel exposing (..)

import Browser
import Html
import Html.Events as Events


port logReceived : (String -> msg) -> Sub msg


type alias Flags =
    ()


type alias Model =
    List String


type Msg
    = LogReceived String
    | Clear


init flags =
    ( [], Cmd.none )


update msg model =
    case msg of
        LogReceived log ->
            ( log :: model, Cmd.none )

        Clear ->
            ( [], Cmd.none )


subscriptions model =
    logReceived LogReceived


view model =
    let
        messages =
            List.map (\str -> Html.li [] [ Html.text str ]) model
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

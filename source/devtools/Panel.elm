port module Panel exposing (..)

import Browser
import Html exposing (Html)
import Html.Events as Events
import Murmur3


port logReceived : (String -> msg) -> Sub msg


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


type Msg
    = LogReceived String
    | Clear


init : Flags -> ( Model, Cmd Msg )
init flags =
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

                messages =
                    if lastHash == Just hash then
                        incrementLastMessageCount model.messages

                    else
                        { count = 1, message = log, hash = hash } :: model.messages
            in
            ( { model | messages = messages }, Cmd.none )

        Clear ->
            ( { messages = []
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    logReceived LogReceived


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

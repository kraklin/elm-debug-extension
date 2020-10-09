port module Panel exposing (main)

import Browser
import Decode exposing (DecodedLog, ElmValue)
import Html exposing (Html)
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Extra as Decode
import Murmur3


port logReceived : (String -> msg) -> Sub msg


port parse : ( Int, String ) -> Cmd msg


port parsedReceived : (Value -> msg) -> Sub msg


type alias Flags =
    ()


type alias DebugMessage =
    { count : Int
    , tag : String
    , value : ElmValue
    , hash : Int
    }


type alias Model =
    { messages : List DebugMessage
    }


type Msg
    = LogReceived String
    | Clear
    | ParsedReceived Value


init : Flags -> ( Model, Cmd Msg )
init _ =
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

                ( messages, cmd ) =
                    if lastHash == Just hash then
                        ( incrementLastMessageCount model.messages, Cmd.none )

                    else
                        ( model.messages, parse ( hash, log ) )
            in
            ( { model | messages = messages }, cmd )

        Clear ->
            ( { messages = []
              }
            , Cmd.none
            )

        ParsedReceived parsedValue ->
            let
                decodedValue =
                    Decode.decodeParsedValue parsedValue

                messages =
                    case decodedValue of
                        Ok { hash, tag, value } ->
                            { count = 1, tag = tag, value = value, hash = hash } :: model.messages

                        Err e ->
                            model.messages
            in
            ( { model | messages = messages }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ logReceived LogReceived
        , parsedReceived ParsedReceived
        ]


viewValue value =
    case value of
        Decode.ElmString str ->
            Html.text str

        Decode.ElmFloat float ->
            Html.text <| String.fromFloat float ++ "f"

        Decode.ElmInt int ->
            Html.text <| String.fromInt int

        Decode.ElmSequence seqType children ->
            let
                typeToString =
                    case seqType of
                        Decode.Tuple ->
                            "(T)"

                        Decode.Set ->
                            "(S)"

                        Decode.List ->
                            "(L)"

                        Decode.Array ->
                            "(A)"
            in
            Html.ul [] <| Html.text typeToString :: List.map (\c -> Html.li [] [ viewValue c ]) children

        Decode.ElmBool bool ->
            Html.text <|
                if bool then
                    "True"

                else
                    "False"

        Decode.ElmRecord recordValues ->
            Html.ul [] <|
                List.map (\( key, child ) -> Html.li [] [ Html.text (key ++ ": "), viewValue child ]) recordValues

        Decode.ElmDict dictValues ->
            Html.ul [] <|
                List.map (\( key, dictValue ) -> Html.li [] [ viewValue key, viewValue dictValue ]) dictValues

        Decode.ElmInternals ->
            Html.text "<internals>"

        Decode.ElmFunction ->
            Html.text "<function>"

        Decode.ElmType name maybeValues ->
            case maybeValues of
                Nothing ->
                    Html.text name

                Just [ oneValue ] ->
                    Html.div [] [ Html.text name, viewValue oneValue ]

                Just multipleValues ->
                    Html.div [] [ Html.text name, Html.ul [] <| List.map (\c -> Html.li [] [ viewValue c ]) multipleValues ]

        Decode.ElmUnit ->
            Html.text "()"

        Decode.ElmFile name ->
            Html.text name

        Decode.ElmBytes count ->
            Html.text <| String.fromInt count ++ "B"


view : Model -> Html Msg
view model =
    let
        messages =
            List.map
                (\{ tag, value, count } ->
                    Html.li []
                        [ Html.text <| "(" ++ String.fromInt count ++ ") " ++ tag
                        , viewValue value
                        ]
                )
                model.messages
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

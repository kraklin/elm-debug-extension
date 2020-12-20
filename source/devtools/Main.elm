module Main exposing (main)

import Browser
import DebugParser
import Expandable
import Html.Events.Extra as Events
import Html.Styled as Html exposing (Html)
import Html.Styled.Events as Events
import Json.Decode exposing (Value)


type alias Model =
    { parsedValue : Maybe DebugParser.ParsedLog
    , input : String
    }


type Msg
    = Toggle Expandable.Key
    | InputChanged String
    | DoTheMagic


init =
    ( { parsedValue = Nothing
      , input = ""
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputChanged str ->
            ( { model | input = str }, Cmd.none )

        DoTheMagic ->
            ( { model
                | parsedValue =
                    DebugParser.parse model.input
                        |> Result.toMaybe
              }
            , Cmd.none
            )

        Toggle path ->
            let
                updatedParsed parsed =
                    { parsed | value = updatedElmValue parsed.value }

                updatedElmValue val =
                    Expandable.map path Expandable.toggle val
            in
            ( { model | parsedValue = Maybe.map updatedParsed model.parsedValue }, Cmd.none )

        _ ->
            ( model, Cmd.none )


subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.h1 [] [ Html.text "Debug.parse" ]
        , Html.textarea [ Events.onInput InputChanged ] []
        , Html.button [ Events.onClick DoTheMagic ] [ Html.text "Do the magic" ]
        , Html.div []
            [ Maybe.map (\parsed -> Expandable.viewMessageHeader Toggle 1 parsed.tag parsed.value) model.parsedValue |> Maybe.withDefault (Html.text "")
            ]
        ]


main : Program Value Model Msg
main =
    Browser.element
        { init = \_ -> init
        , update = update
        , view = view >> Html.toUnstyled
        , subscriptions = subscriptions
        }

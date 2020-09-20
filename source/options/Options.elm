port module Options exposing (..)

import Browser
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Form.Decoder as Decoder exposing (Decoder)
import Html exposing (Html)



-- PORTS FROM JAVASCRIPT


port saveGlobalOptions : GlobalOptions -> Cmd msg


port globalsSavedResult : (Maybe String -> msg) -> Sub msg


type alias Flags =
    { version : String
    , hasCustomFormatters : Bool
    , initialOptions : GlobalOptions
    }


type alias GlobalOptions =
    { simple_mode : Bool
    , limit : Int
    , debug : Bool
    }


type Mode
    = Simple
    | CustomFormatters


type alias Form =
    { limit : String
    , debug : Bool
    , mode : Mode
    }


type Error
    = InvalidLimit


limitDecoder : Decoder String Error Int
limitDecoder =
    Decoder.identity
        |> Decoder.pass (Decoder.int InvalidLimit)
        |> Decoder.assert (Decoder.minBound InvalidLimit 0)


debugDecoder : Decoder Bool Error Bool
debugDecoder =
    Decoder.identity


modeDecoder : Decoder Mode Error Bool
modeDecoder =
    Decoder.custom <|
        \mode ->
            case mode of
                Simple ->
                    Ok True

                CustomFormatters ->
                    Ok False


formDecoder : Decoder Form Error GlobalOptions
formDecoder =
    Decoder.top GlobalOptions
        |> Decoder.field (Decoder.lift .mode modeDecoder)
        |> Decoder.field (Decoder.lift .limit limitDecoder)
        |> Decoder.field (Decoder.lift .debug debugDecoder)


optionsToForm : GlobalOptions -> Form
optionsToForm { limit, simple_mode, debug } =
    { limit = String.fromInt limit
    , debug = debug
    , mode =
        if simple_mode then
            Simple

        else
            CustomFormatters
    }


type SavedResult
    = NotSavedYet
    | SavedOk
    | SavedError String


type alias Model =
    { options : Result (List Error) GlobalOptions
    , flags : Flags
    , form : Form
    , savedResult : SavedResult
    }


type alias Request =
    { action : String }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { flags = flags
      , options = Ok flags.initialOptions
      , form = optionsToForm flags.initialOptions
      , savedResult = NotSavedYet
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | SetDebug Bool
    | SetSimpleMode Mode
    | UpdateLimitForm String
    | ValidateForm
    | SaveChanges
    | GlobalsSavedResult (Maybe String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        setFormValue : Form -> Model
        setFormValue a =
            { model | form = a, savedResult = NotSavedYet }

        modelForm =
            model.form
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetDebug newDebug ->
            ( setFormValue { modelForm | debug = newDebug }, Cmd.none )

        SetSimpleMode newMode ->
            ( setFormValue { modelForm | mode = newMode }, Cmd.none )

        UpdateLimitForm limitString ->
            ( setFormValue { modelForm | limit = limitString }, Cmd.none )

        ValidateForm ->
            ( { model | options = Decoder.run formDecoder model.form }, Cmd.none )

        SaveChanges ->
            let
                validatedOptions =
                    Decoder.run formDecoder model.form

                saveMessage =
                    case validatedOptions of
                        Ok options ->
                            saveGlobalOptions options

                        -- TODO: save options port
                        Err _ ->
                            Cmd.none
            in
            ( { model | options = validatedOptions }, saveMessage )

        GlobalsSavedResult maybeErrors ->
            case maybeErrors of
                Nothing ->
                    ( { model | savedResult = SavedOk }, Cmd.none )

                Just err ->
                    ( { model | savedResult = SavedError err }, Cmd.none )



-- VIEW


dark : Element.Color
dark =
    Element.rgb255 52 73 94


light : Element.Color
light =
    Element.rgb255 252 253 254


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


lightGrey : Element.Color
lightGrey =
    Element.rgb255 220 220 220


red : Element.Color
red =
    Element.rgb255 181 75 59


header : String -> Element Msg
header version =
    Element.row
        [ Element.width Element.fill
        , Element.spacing 8
        , Element.paddingXY 0 8
        , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
        ]
        [ Element.image
            [ Element.width <| Element.px 24 ]
            { description = "Elm Debug Extension Logo", src = "/assets/elm-logo.png" }
        , Element.el
            [ Font.size 20
            , Font.medium
            , Font.color dark
            ]
            (Element.text <| String.toUpper "Elm Debug Extension")
        , Element.el
            [ Font.size 20
            , Font.regular
            , Font.color dark
            , Element.alignRight
            ]
            (Element.text <| String.toUpper "Settings")
        ]


section : String -> Element Msg -> Element Msg -> Element Msg
section name sectionContent help =
    Element.row
        [ Background.color light
        , Element.padding 20
        , Element.width Element.fill
        , Element.spacing 4
        , Border.rounded 8
        , Border.shadow { offset = ( 0, 2 ), size = 2, blur = 4, color = lightGrey }
        ]
        [ Element.column [ Element.width Element.fill, Element.spacing 20 ]
            [ Element.el
                [ Font.size 18
                , Font.regular
                , Font.color blue
                ]
                (Element.text <| String.toUpper name)
            , Element.row
                [ Element.width Element.fill
                , Element.spacing 20
                , Element.paddingEach { top = 0, left = 20, right = 0, bottom = 0 }
                ]
                [ sectionContent
                , Element.el [ Element.width <| Element.px 350 ] help
                ]
            ]
        ]


parserLimitSettings : List Error -> String -> Element Msg
parserLimitSettings errors limitValue =
    let
        borderStyle =
            if List.member InvalidLimit errors then
                [ Border.color red, Border.width 2 ]

            else
                [ Border.color dark ]

        showError =
            if List.member InvalidLimit errors then
                Element.el [ Font.color red, Font.bold, Element.alignRight ] <| Element.text "Please, use only positive integers"

            else
                Element.none

        limitForm =
            Element.column [ Element.width Element.fill, Element.height Element.fill ]
                [ Input.text ([ Element.alignTop, Events.onLoseFocus ValidateForm ] ++ borderStyle)
                    { label = Input.labelAbove [] (Element.text "Don't parse output after [# chars]")
                    , onChange = UpdateLimitForm
                    , placeholder = Nothing
                    , text = limitValue
                    }
                , showError
                ]

        help =
            Element.column [ Element.width Element.fill, Element.spacing 12 ]
                [ Element.paragraph [ Font.size 12, Font.color grey ]
                    [ Element.text "Large messages takes longer to be parsed. But in some cases you need them anyway." ]
                , Element.paragraph [ Font.size 12, Font.color grey ]
                    [ Element.text "Here you can set the maximum number of characters that is going to be parsed. " ]
                , Element.paragraph [ Font.size 12, Font.color grey ]
                    [ Element.text "If the message is longer than the limit, it is not parsed and it is sent to the console as it was thrown out of the system."
                    ]
                ]
    in
    section "Parser limit" limitForm help


simpleModeSettings : Mode -> Element Msg
simpleModeSettings simpleMode =
    let
        optionBaseStyle =
            [ Element.padding 8
            , Border.color dark
            , Border.width 1
            , Border.rounded 3
            , Background.color light
            ]

        optionElement text state =
            case state of
                Input.Idle ->
                    Input.button optionBaseStyle { label = Element.text text, onPress = Nothing }

                Input.Focused ->
                    Input.button (optionBaseStyle ++ [ Border.color dark ]) { label = Element.text text, onPress = Nothing }

                Input.Selected ->
                    Input.button (optionBaseStyle ++ [ Background.color dark, Font.color light ]) { label = Element.text text, onPress = Nothing }

        option value text =
            Input.optionWith value (optionElement text)

        simpleSettingsForm =
            Input.radioRow [ Element.width Element.fill, Element.spacing 20 ]
                { label = Input.labelHidden "Select simple mode or custom formatter mode"
                , onChange = SetSimpleMode
                , options =
                    [ option Simple "simple"
                    , option CustomFormatters "custom formatter"
                    ]
                , selected = Just simpleMode
                }

        help =
            Element.column [ Element.width Element.fill, Element.spacing 12 ]
                [ Element.paragraph [ Font.size 12, Font.color grey ]
                    [ Element.text "Chromium based browsers have ability to use custom formats in console. This option can turn this kind of formatting on." ]
                , Element.paragraph [ Font.size 12, Font.color dark, Font.bold ]
                    [ Element.text "There is an extra step you need to do to enable custom formatting in console." ]
                , Element.paragraph [ Font.size 12, Font.color grey ]
                    [ Element.text "Open your Developers console > hit F1 > in Preferences navigate to Console > turn on option Enable custom formatters." ]
                ]
    in
    section "Output object shape" simpleSettingsForm help


debugModeSettings : Bool -> Element Msg
debugModeSettings debugTurnedOn =
    let
        checkbox value =
            if value then
                Input.button
                    [ Element.padding 2
                    , Element.width <| Element.px 20
                    , Element.height <| Element.px 20
                    , Border.color dark
                    , Border.width 1
                    , Border.rounded 3
                    , Background.color dark
                    , Font.color light
                    ]
                    { label = Element.text "✓", onPress = Nothing }

            else
                Input.button
                    [ Element.padding 4
                    , Element.width <| Element.px 20
                    , Element.height <| Element.px 20
                    , Border.color dark
                    , Border.width 1
                    , Border.rounded 3
                    , Background.color light
                    , Font.color dark
                    ]
                    { label = Element.text " ", onPress = Nothing }

        debugForm =
            Element.row [ Element.spacing 20 ]
                [ Input.checkbox [] { onChange = SetDebug, icon = checkbox, checked = debugTurnedOn, label = Input.labelLeft [] (Element.text "Debug mode") }
                ]
    in
    section "Debug mode" debugForm Element.none


saveButton : SavedResult -> Element Msg
saveButton savedResult =
    Element.column [ Element.spacing 20, Element.width Element.fill ]
        [ Input.button
            [ Background.color green
            , Font.color light
            , Element.padding 8
            , Element.alignRight
            , Border.rounded 4
            ]
            { label = Element.text "Save settings", onPress = Just SaveChanges }
        , case savedResult of
            NotSavedYet ->
                Element.none

            SavedOk ->
                Element.el [ Font.color green, Element.alignRight ] <| Element.text "Setting has been saved successfuly"

            SavedError err ->
                Element.el [ Font.color red, Element.alignRight ] <| Element.text <| "Error: " ++ err
        ]


content : Model -> Element Msg
content model =
    let
        limitErrors =
            case model.options of
                Ok _ ->
                    []

                Err e ->
                    e
    in
    Element.el [ Element.width Element.fill ] <|
        Element.column
            [ Element.width <| Element.maximum 700 Element.fill
            , Element.centerX
            , Element.spacing 16
            ]
            [ header model.flags.version
            , parserLimitSettings limitErrors model.form.limit
            , if model.flags.hasCustomFormatters then
                simpleModeSettings model.form.mode

              else
                Element.none
            , debugModeSettings model.form.debug
            , saveButton model.savedResult
            ]


view : Model -> Html Msg
view model =
    Element.layout
        [ Element.width Element.fill
        , Element.padding 12
        , Font.family
            [ Font.typeface "IBM Plex Sans"
            , Font.sansSerif
            ]
        , Font.size 14
        , Font.color dark
        ]
        (content model)


subscriptions : Model -> Sub Msg
subscriptions model =
    globalsSavedResult GlobalsSavedResult


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

module Expandable exposing
    ( ElmValue
    , Key
    , decodeParsedValue
    , map
    , toggle
    , viewValue
    )

import Html exposing (Html)
import Html.Events as Events
import Html.Events.Extra as Events
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Extra as Decode
import List.Extra as List


type SequenceType
    = Set
    | List
    | Array


type ElmValue
    = ElmString String
    | ElmInt Int
    | ElmFloat Float
    | ElmBool Bool
    | ElmFunction
    | ElmInternals
    | ElmUnit
    | ElmFile String
    | ElmBytes Int
    | ElmSequence SequenceType Bool (List ElmValue)
    | ElmTuple Bool (List ElmValue)
    | ElmRecord Bool (List ( String, ElmValue ))
    | ElmType Bool String (Maybe (List ElmValue))
    | ElmDict Bool (List ( ElmValue, ElmValue ))


type alias Key =
    List Int


type alias DecodedLog =
    { tag : String
    , value : ElmValue
    , hash : Int
    }


andDecodeValueField : Decoder a -> Decoder (a -> b) -> Decoder b
andDecodeValueField decoder =
    Decode.andMap (Decode.field "value" decoder)


valueDecoder : Decoder ElmValue
valueDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\valueType ->
                case valueType of
                    "String" ->
                        Decode.succeed ElmString
                            |> andDecodeValueField Decode.string

                    "Number" ->
                        Decode.oneOf
                            [ Decode.succeed ElmInt
                                |> andDecodeValueField Decode.int
                            , Decode.succeed ElmFloat
                                |> andDecodeValueField Decode.float
                            ]

                    "Boolean" ->
                        Decode.succeed ElmBool
                            |> andDecodeValueField Decode.bool

                    "Tuple" ->
                        Decode.succeed (ElmTuple False)
                            |> andDecodeValueField (Decode.list valueDecoder)

                    "Set" ->
                        Decode.succeed (ElmSequence Set False)
                            |> andDecodeValueField (Decode.list valueDecoder)

                    "List" ->
                        Decode.succeed (ElmSequence List False)
                            |> andDecodeValueField (Decode.list valueDecoder)

                    "Array" ->
                        Decode.succeed (ElmSequence Array False)
                            |> andDecodeValueField (Decode.list valueDecoder)

                    "Record" ->
                        Decode.succeed (ElmRecord False)
                            |> andDecodeValueField (Decode.keyValuePairs valueDecoder)

                    "Internals" ->
                        Decode.succeed ElmInternals

                    "Function" ->
                        Decode.succeed ElmFunction

                    "Unit" ->
                        Decode.succeed ElmUnit

                    "Type" ->
                        Decode.succeed (ElmType False)
                            |> Decode.andMap (Decode.field "name" Decode.string)
                            |> Decode.andMap (Decode.succeed Nothing)

                    "Custom" ->
                        Decode.succeed (ElmType False)
                            |> Decode.andMap (Decode.field "name" Decode.string)
                            |> Decode.andMap (Decode.maybe (Decode.field "value" (Decode.list valueDecoder)))

                    "Dict" ->
                        Decode.succeed (ElmDict False)
                            |> andDecodeValueField (Decode.list (Decode.map2 Tuple.pair (Decode.field "key" valueDecoder) (Decode.field "value" valueDecoder)))

                    "File" ->
                        Decode.succeed ElmFile
                            |> andDecodeValueField Decode.string

                    "Bytes" ->
                        Decode.succeed ElmBytes
                            |> andDecodeValueField Decode.int

                    _ ->
                        Decode.succeed <| ElmString "<unable to decode value>"
            )


elmValueDecoder : Decoder ElmValue
elmValueDecoder =
    Decode.oneOf
        [ valueDecoder
        , Decode.succeed <|
            ElmString "<unknown value>"
        ]


logDecoder : Decoder DecodedLog
logDecoder =
    Decode.succeed DecodedLog
        |> Decode.andMap (Decode.at [ "log", "name" ] Decode.string)
        |> Decode.andMap (Decode.at [ "log", "value" ] elmValueDecoder)
        |> Decode.andMap (Decode.field "hash" Decode.int)


decodeParsedValue : Value -> Result Decode.Error DecodedLog
decodeParsedValue value =
    Decode.decodeValue logDecoder value



-- Data structure actions


hasNestedValues : ElmValue -> Bool
hasNestedValues value =
    case value of
        ElmSequence _ _ values ->
            not <| List.isEmpty values

        ElmTuple _ _ ->
            True

        ElmRecord _ _ ->
            True

        ElmType _ _ maybeValues ->
            maybeValues /= Nothing

        ElmDict _ values ->
            not <| List.isEmpty values

        _ ->
            False


toggle : ElmValue -> ElmValue
toggle value =
    case value of
        ElmSequence seq isOpened values ->
            ElmSequence seq (not isOpened) values

        ElmDict isOpened values ->
            ElmDict (not isOpened) values

        ElmType isOpened name maybeValues ->
            case maybeValues of
                Nothing ->
                    value

                Just [] ->
                    value

                Just _ ->
                    ElmType (not isOpened) name maybeValues

        ElmRecord isOpened values ->
            ElmRecord (not isOpened) values

        _ ->
            value


mapNestedValue : Int -> (ElmValue -> ElmValue) -> ElmValue -> ElmValue
mapNestedValue mapIndex fn value =
    case value of
        ElmRecord a values ->
            ElmRecord a <| List.updateIfIndex ((==) mapIndex) (Tuple.mapSecond fn) values

        ElmSequence a b values ->
            ElmSequence a b <| List.updateIfIndex ((==) mapIndex) fn values

        ElmType a b maybeValues ->
            case maybeValues of
                Nothing ->
                    value

                Just values ->
                    ElmType a b <| Just <| List.updateIfIndex ((==) mapIndex) fn values

        ElmDict a values ->
            ElmDict a <| List.updateIfIndex ((==) mapIndex) (Tuple.mapSecond fn) values

        _ ->
            value


map : Key -> (ElmValue -> ElmValue) -> ElmValue -> ElmValue
map key fn value =
    case key of
        [] ->
            fn value

        [ idx ] ->
            mapNestedValue idx fn value

        idx :: rest ->
            mapNestedValue idx (map rest fn) value



-- VIEW --


viewValueHeader : ElmValue -> Html msg
viewValueHeader value =
    case value of
        ElmTuple _ children ->
            Html.span []
                [ Html.span []
                    [ Html.text "("
                    , Html.span []
                        (List.map viewValueHeader children
                            |> List.intersperse (Html.text ", ")
                        )
                    , Html.text ")"
                    ]
                ]

        ElmSequence seqType _ children ->
            let
                typeToString =
                    case seqType of
                        Set ->
                            "Set"

                        List ->
                            "List"

                        Array ->
                            "Array"
            in
            case children of
                [] ->
                    Html.text <| "[]"

                [ singleton ] ->
                    Html.span [] <|
                        if hasNestedValues singleton then
                            [ Html.span []
                                [ Html.text "[...]" ]
                            ]

                        else
                            [ Html.text "[ "
                            , viewValueHeader singleton
                            , Html.text " ]"
                            ]

                _ ->
                    Html.text <| typeToString ++ "(" ++ String.fromInt (List.length children) ++ ")"

        ElmRecord _ recordValues ->
            Html.span
                []
                [ case recordValues of
                    [] ->
                        Html.text "{}"

                    [ ( name, singleValue ) ] ->
                        Html.span []
                            [ Html.text "{"
                            , Html.text <| name ++ ": "
                            , viewValueHeader singleValue
                            , Html.text "}"
                            ]

                    ( name, firstItem ) :: _ ->
                        Html.span []
                            [ Html.text "{"
                            , Html.text <| name ++ ": "
                            , viewValueHeader firstItem
                            , Html.text ", ...}"
                            ]
                ]

        ElmDict _ dictValues ->
            Html.span
                []
                [ Html.text <| "Dict (" ++ (String.fromInt <| List.length dictValues) ++ ")" ]

        ElmType _ name maybeValues ->
            case maybeValues of
                Nothing ->
                    Html.text name

                Just [ oneValue ] ->
                    Html.span [] [ Html.text name, viewValueHeader oneValue ]

                Just _ ->
                    Html.span
                        []
                        [ Html.text name, Html.text " ..." ]

        ElmString str ->
            Html.text <| "\"" ++ str ++ "\""

        ElmFloat float ->
            Html.text <| String.fromFloat float ++ "f"

        ElmInt int ->
            Html.text <| String.fromInt int

        ElmBool bool ->
            Html.text <|
                if bool then
                    "True"

                else
                    "False"

        ElmInternals ->
            Html.text "<internals>"

        ElmFunction ->
            Html.text "<function>"

        ElmUnit ->
            Html.text "()"

        ElmFile name ->
            Html.text name

        ElmBytes count ->
            Html.text <| String.fromInt count ++ "B"


viewValue : (Key -> msg) -> Key -> ElmValue -> Html msg
viewValue toggleMsg parentKey value =
    let
        viewChildFn idx =
            viewValue toggleMsg (parentKey ++ [ idx ])

        viewFn =
            viewValue toggleMsg parentKey
    in
    case value of
        ElmTuple isOpened children ->
            Html.span []
                [ Html.span []
                    [ Html.text "("
                    , Html.span []
                        (List.map viewFn children
                            |> List.intersperse (Html.text ", ")
                        )
                    , Html.text ")"
                    ]
                , if isOpened then
                    Html.ul [] <|
                        List.map
                            (\v ->
                                Html.li
                                    []
                                    [ viewFn v ]
                            )
                            children

                  else
                    Html.text ""
                ]

        ElmSequence _ isOpened children ->
            Html.span []
                [ viewValueHeader value
                , if isOpened then
                    Html.ul [] <|
                        List.indexedMap
                            (\idx c ->
                                Html.li
                                    [ Events.onClickStopPropagation <| toggleMsg (parentKey ++ [ idx ]) ]
                                    [ viewChildFn idx c
                                    ]
                            )
                            children

                  else
                    Html.text ""
                ]

        ElmRecord isOpened recordValues ->
            Html.span []
                [ viewValueHeader value
                , if isOpened then
                    recordValues
                        |> List.indexedMap
                            (\idx ( key, child ) ->
                                Html.div
                                    [ Events.onClickStopPropagation <| toggleMsg (parentKey ++ [ idx ]) ]
                                    [ Html.text (key ++ ": "), viewChildFn idx child ]
                            )
                        |> Html.div []

                  else
                    Html.text ""
                ]

        ElmDict isOpened dictValues ->
            Html.span []
                [ viewValueHeader value
                , if isOpened then
                    Html.ul [] <|
                        List.indexedMap
                            (\idx ( key, dictValue ) ->
                                Html.li
                                    [ Events.onClickStopPropagation <| toggleMsg <| parentKey ++ [ idx ] ]
                                    [ viewFn key, viewChildFn idx dictValue ]
                            )
                            dictValues

                  else
                    Html.text ""
                ]

        ElmType isOpened name maybeValues ->
            case maybeValues of
                Nothing ->
                    Html.text name

                Just [ oneValue ] ->
                    Html.span [] [ Html.text name, viewFn oneValue ]

                Just multipleValues ->
                    if isOpened then
                        Html.span
                            [ Events.onClickStopPropagation <| toggleMsg <| parentKey ]
                            [ Html.text name, Html.ul [] <| List.indexedMap (\idx c -> Html.li [] [ viewChildFn idx c ]) multipleValues ]

                    else
                        Html.span
                            [ Events.onClickStopPropagation <| toggleMsg <| parentKey ]
                            [ Html.text name, Html.text " ..." ]

        _ ->
            viewValueHeader value

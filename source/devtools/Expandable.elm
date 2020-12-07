module Expandable exposing
    ( ElmValue(..)
    , Key
    , SequenceType(..)
    , decodeParsedValue
    , logDecoder
    , map
    , toggle
    , viewMessageHeader
    , viewValue
    )

import Css
import Html.Events as Events
import Html.Events.Extra as Events
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Extra as Decode
import List.Extra as List


type SequenceType
    = Set
    | List
    | Array


type ElmValue
    = ElmString String
    | ElmChar Char
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
    | ElmType Bool String (List ElmValue)
    | ElmDict Bool (List ( ElmValue, ElmValue ))


type alias Key =
    List Int


type alias DecodedLog =
    { tag : String
    , value : ElmValue
    , hash : Int
    , isoTimestamp : String
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
                            |> Decode.andMap (Decode.succeed [])

                    "Custom" ->
                        Decode.succeed (ElmType False)
                            |> Decode.andMap (Decode.field "name" Decode.string)
                            |> Decode.andMap (Decode.field "value" (Decode.list valueDecoder))

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
        |> Decode.andMap (Decode.field "timestamp" Decode.string)


decodeParsedValue : Value -> Result Decode.Error DecodedLog
decodeParsedValue value =
    Decode.decodeValue logDecoder value



-- Data structure actions


hasNestedValues : ElmValue -> Bool
hasNestedValues value =
    case value of
        ElmTuple _ _ ->
            True

        ElmSequence _ _ values ->
            not <| List.isEmpty values

        ElmRecord _ _ ->
            True

        ElmDict _ values ->
            not <| List.isEmpty values

        ElmType _ _ values ->
            not <| List.isEmpty values

        _ ->
            False


toggle : ElmValue -> ElmValue
toggle value =
    case value of
        ElmTuple isOpened values ->
            ElmTuple (not isOpened) values

        ElmSequence seq isOpened values ->
            ElmSequence seq (not isOpened) values

        ElmRecord isOpened values ->
            ElmRecord (not isOpened) values

        ElmDict isOpened values ->
            ElmDict (not isOpened) values

        ElmType isOpened name values ->
            case values of
                [] ->
                    value

                _ ->
                    ElmType (not isOpened) name values

        _ ->
            value


map : Key -> (ElmValue -> ElmValue) -> ElmValue -> ElmValue
map key fn value =
    let
        mapNestedValue mapIndex mappedFn =
            case value of
                ElmTuple a values ->
                    ElmTuple a <| List.updateIfIndex ((==) mapIndex) mappedFn values

                ElmSequence a b values ->
                    ElmSequence a b <| List.updateIfIndex ((==) mapIndex) mappedFn values

                ElmRecord a values ->
                    ElmRecord a <| List.updateIfIndex ((==) mapIndex) (Tuple.mapSecond mappedFn) values

                ElmDict a values ->
                    ElmDict a <| List.updateIfIndex ((==) mapIndex) (Tuple.mapSecond mappedFn) values

                ElmType a b typeValues ->
                    case typeValues of
                        [] ->
                            value

                        values ->
                            ElmType a b <| List.updateIfIndex ((==) mapIndex) mappedFn values

                _ ->
                    value
    in
    case key of
        [] ->
            fn value

        [ idx ] ->
            mapNestedValue idx fn

        idx :: rest ->
            mapNestedValue idx (map rest fn)



-- VIEW --


theme =
    { stringColor = Css.hex "0000ff"
    , internalsColor = Css.hex "808080"
    , keysColor = Css.hex "ff00ff"
    , guidelinesColor = Css.hex "a0a0a0"
    , expandTriangleColor = Css.hex "808080"
    }


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
                            , Html.span [ Attrs.css [ Css.color theme.keysColor ] ] [ Html.text name ]
                            , Html.text ": "
                            , viewValueHeader singleValue
                            , Html.text "}"
                            ]

                    ( name, firstItem ) :: _ ->
                        Html.span []
                            [ Html.text "{"
                            , Html.span [ Attrs.css [ Css.color theme.keysColor ] ] [ Html.text name ]
                            , Html.text ": "
                            , viewValueHeader firstItem
                            , Html.text ", ...}"
                            ]
                ]

        ElmDict _ dictValues ->
            Html.span
                []
                [ Html.text <| "Dict (" ++ (String.fromInt <| List.length dictValues) ++ ")" ]

        ElmType _ name values ->
            case values of
                [] ->
                    Html.text name

                [ oneValue ] ->
                    Html.span [] [ Html.text (name ++ " "), viewValueHeader oneValue ]

                _ ->
                    Html.span
                        []
                        [ Html.text name, Html.text " ..." ]

        ElmString str ->
            Html.span [ Attrs.css [ Css.color theme.stringColor ] ] [ Html.text <| "\"" ++ str ++ "\"" ]

        ElmChar str ->
            Html.span [ Attrs.css [ Css.color theme.stringColor ] ] [ Html.text <| "'" ++ String.fromChar str ++ "'" ]

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
            Html.span [ Attrs.css [ Css.color theme.internalsColor ] ]
                [ Html.text "<internals>" ]

        ElmFunction ->
            Html.span [ Attrs.css [ Css.color theme.internalsColor ] ]
                [ Html.text "<function>" ]

        ElmUnit ->
            Html.text "()"

        ElmFile name ->
            Html.text name

        ElmBytes count ->
            Html.text <| String.fromInt count ++ "B"


isValueOpened : ElmValue -> Bool
isValueOpened value =
    case value of
        ElmTuple isOpened _ ->
            isOpened

        ElmSequence _ isOpened _ ->
            isOpened

        ElmRecord isOpened _ ->
            isOpened

        ElmDict isOpened _ ->
            isOpened

        ElmType isOpened _ _ ->
            isOpened

        _ ->
            False


toggableDiv child toggleAttribute content =
    if hasNestedValues child then
        Html.div [ toggleAttribute ] <|
            Html.span []
                [ Html.span
                    [ Attrs.css
                        [ Css.display Css.inlineBlock
                        , Css.cursor Css.pointer
                        , Css.width (Css.px 12)
                        , Css.color theme.expandTriangleColor
                        ]
                    ]
                    [ if isValueOpened child then
                        Html.text "▾"

                      else
                        Html.text "▸"
                    ]
                ]
                :: content

    else
        Html.div [ Attrs.css [ Css.marginLeft <| Css.px 12 ] ] content


viewMessageHeader : (Key -> msg) -> Int -> String -> ElmValue -> Html msg
viewMessageHeader toggleMsg count tag value =
    let
        viewCount =
            if count > 1 then
                Html.span
                    [ Attrs.css
                        [ Css.display Css.inlineBlock
                        , Css.color <| Css.hex "ffffff"
                        , Css.backgroundColor <| Css.hex "ff00ff"
                        , Css.textAlign <| Css.center
                        , Css.borderRadius <| Css.px 14
                        , Css.padding2 (Css.px 0) (Css.px 8)
                        , Css.fontSize <| Css.px 10
                        , Css.marginRight (Css.px 4)
                        ]
                    ]
                    [ Html.text <| String.fromInt count ]

            else
                Html.text ""
    in
    Html.div []
        [ viewCount
        , Html.text tag
        , toggableDiv value
            (Attrs.fromUnstyled <| Events.onClickStopPropagation <| toggleMsg [])
            [ viewValue toggleMsg [] value ]
        ]


viewValue : (Key -> msg) -> Key -> ElmValue -> Html msg
viewValue toggleMsg parentKey value =
    let
        viewChildFn idx v =
            Html.span [] [ viewValue toggleMsg (parentKey ++ [ idx ]) v ]

        toggleCurrent idx =
            Attrs.fromUnstyled <| Events.onClickStopPropagation <| toggleMsg (parentKey ++ [ idx ])

        viewFn =
            viewValue toggleMsg parentKey

        childrenWrapper attrs children =
            Html.div
                ([ Attrs.css
                    [ Css.paddingLeft (Css.em 1)
                    , Css.marginLeft (Css.px 3)
                    , Css.borderLeft3 (Css.px 1) Css.solid theme.guidelinesColor
                    ]
                 ]
                    ++ attrs
                )
                children
    in
    case value of
        ElmTuple isOpened children ->
            Html.span []
                [ viewValueHeader value
                , if isOpened then
                    childrenWrapper [] <|
                        List.indexedMap
                            (\idx child ->
                                toggableDiv child (toggleCurrent idx) [ viewChildFn idx child ]
                            )
                            children

                  else
                    Html.text ""
                ]

        ElmSequence _ isOpened children ->
            Html.span []
                [ viewValueHeader value
                , if isOpened then
                    childrenWrapper [] <|
                        List.indexedMap
                            (\idx child ->
                                toggableDiv child (toggleCurrent idx) [ viewChildFn idx child ]
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
                                toggableDiv child
                                    (toggleCurrent idx)
                                    [ Html.span [ Attrs.css [ Css.color theme.keysColor ] ]
                                        [ Html.text key ]
                                    , Html.text ": "
                                    , viewChildFn idx child
                                    ]
                            )
                        |> childrenWrapper []

                  else
                    Html.text ""
                ]

        ElmDict isOpened dictValues ->
            Html.span []
                [ viewValueHeader value
                , if isOpened then
                    childrenWrapper [] <|
                        List.indexedMap
                            (\idx ( key, dictValue ) ->
                                toggableDiv dictValue
                                    (toggleCurrent idx)
                                    [ viewFn key, Html.text ": ", viewChildFn idx dictValue ]
                            )
                            dictValues

                  else
                    Html.text ""
                ]

        ElmType isOpened name values ->
            case values of
                [] ->
                    Html.text name

                [ oneValue ] ->
                    Html.span []
                        [ viewValueHeader value
                        , if isOpened then
                            childrenWrapper [] <|
                                [ toggableDiv oneValue
                                    (toggleCurrent 0)
                                    [ viewChildFn 0 oneValue ]
                                ]

                          else
                            Html.text ""
                        ]

                multipleValues ->
                    if isOpened then
                        Html.span
                            [ Attrs.fromUnstyled <| Events.onClickStopPropagation <| toggleMsg <| parentKey ]
                            [ Html.text name
                            , childrenWrapper [] <|
                                List.indexedMap (\idx c -> toggableDiv c (toggleCurrent idx) [ viewChildFn idx c ]) multipleValues
                            ]

                    else
                        Html.span
                            [ Attrs.fromUnstyled <| Events.onClickStopPropagation <| toggleMsg <| parentKey ]
                            [ Html.text name, Html.text " ..." ]

        _ ->
            viewValueHeader value

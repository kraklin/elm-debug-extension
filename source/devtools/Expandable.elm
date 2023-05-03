module Expandable exposing
    ( decodeParsedValue
    , logDecoder
    , viewMessageHeader
    , viewValue
    )

import Css
import DebugParser.ElmValue as ElmValue
    exposing
        ( ElmValue(..)
        , ExpandableValue(..)
        , Path
        , PlainValue(..)
        , SequenceType(..)
        )
import Html.Events.Extra as Events
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Extra as Decode


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
                        Decode.succeed (Plain << ElmString)
                            |> andDecodeValueField Decode.string

                    "Number" ->
                        Decode.succeed (Plain << ElmNumber)
                            |> andDecodeValueField Decode.float

                    "Boolean" ->
                        Decode.succeed (Plain << ElmBool)
                            |> andDecodeValueField Decode.bool

                    "Tuple" ->
                        Decode.succeed (Expandable False << ElmSequence SeqTuple)
                            |> andDecodeValueField (Decode.list valueDecoder)

                    "Set" ->
                        Decode.succeed (Expandable False << ElmSequence SeqSet)
                            |> andDecodeValueField (Decode.list valueDecoder)

                    "List" ->
                        Decode.succeed (Expandable False << ElmSequence SeqList)
                            |> andDecodeValueField (Decode.list valueDecoder)

                    "Array" ->
                        Decode.succeed (Expandable False << ElmSequence SeqArray)
                            |> andDecodeValueField (Decode.list valueDecoder)

                    "Record" ->
                        Decode.succeed (Expandable False << ElmRecord)
                            |> andDecodeValueField (Decode.keyValuePairs valueDecoder)

                    "Internals" ->
                        Decode.succeed (Plain ElmInternals)

                    "Function" ->
                        Decode.succeed (Plain ElmFunction)

                    "Unit" ->
                        Decode.succeed (Plain ElmUnit)

                    "Type" ->
                        Decode.succeed ElmType
                            |> Decode.andMap (Decode.field "name" Decode.string)
                            |> Decode.andMap (Decode.succeed [])
                            |> Decode.map (Expandable False)

                    "Custom" ->
                        Decode.succeed ElmType
                            |> Decode.andMap (Decode.field "name" Decode.string)
                            |> Decode.andMap (Decode.field "value" (Decode.list valueDecoder))
                            |> Decode.map (Expandable False)

                    "Dict" ->
                        Decode.succeed (Expandable False << ElmDict)
                            |> andDecodeValueField (Decode.list (Decode.map2 Tuple.pair (Decode.field "key" valueDecoder) (Decode.field "value" valueDecoder)))

                    "File" ->
                        Decode.succeed (Plain << ElmFile)
                            |> andDecodeValueField Decode.string

                    "Bytes" ->
                        Decode.succeed (Plain << ElmBytes)
                            |> andDecodeValueField Decode.int

                    _ ->
                        Decode.succeed <| (Plain <| ElmString "<unable to decode value>")
            )


elmValueDecoder : Decoder ElmValue
elmValueDecoder =
    Decode.oneOf
        [ valueDecoder
        , Decode.succeed <|
            (Plain <| ElmString "<unknown value>")
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


isValueExpanded : ElmValue -> Bool
isValueExpanded value =
    case value of
        Expandable isOpened _ ->
            isOpened

        _ ->
            False



-- VIEW --


type alias ColorTheme a =
    { a
        | stringColor : Css.Color
        , internalsColor : Css.Color
        , keysColor : Css.Color
        , guidelinesColor : Css.Color
        , expandTriangleColor : Css.Color
        , valueBackgroundColor : Css.Color
        , numbersColor : Css.Color
        , customTypesColor : Css.Color
        , booleanColor : Css.Color
        , sequenceNameColor : Css.Color
        , primary : Css.Color
    }


viewMessageHeader : ColorTheme a -> (Path -> msg) -> Int -> String -> String -> ElmValue -> Html msg
viewMessageHeader colorTheme toggleMsg count tag time value =
    let
        viewCount =
            if count > 1 then
                Html.span
                    [ Attrs.css
                        [ Css.display Css.inlineBlock
                        , Css.color <| Css.hex "ffffff"
                        , Css.backgroundColor colorTheme.primary
                        , Css.textAlign <| Css.center
                        , Css.borderRadius <| Css.px 14
                        , Css.padding2 (Css.px 0) (Css.px 8)
                        , Css.fontSize <| Css.px 10
                        , Css.marginRight <| Css.px 4
                        ]
                    ]
                    [ Html.text <| String.fromInt count ]

            else
                Html.text ""

        viewTime =
            Html.span
                [ Attrs.css
                    [ Css.fontSize <| Css.px 10
                    , Css.color colorTheme.guidelinesColor
                    , Css.textAlign Css.right
                    ]
                ]
                [ Html.text time ]
    in
    Html.div
        [ Attrs.css
            [ Css.fontFamilies [ "IBM Plex Mono", "monospace" ]
            , Css.fontSize <| Css.px 11
            ]
        ]
        [ Html.div
            [ Attrs.css
                [ Css.displayFlex
                , Css.alignItems Css.baseline
                , Css.marginBottom <| Css.px 4
                ]
            ]
            [ viewCount
            , Html.span
                [ Attrs.css
                    [ Css.flexGrow <| Css.int 1
                    , Css.fontSize <| Css.px 10
                    ]
                ]
                [ Html.text tag ]
            , viewTime
            ]
        , valueHeader colorTheme toggleMsg [] Nothing value
        ]


viewValueHeader : ColorTheme a -> ElmValue -> Html msg
viewValueHeader =
    viewValueHeaderInner 0


viewValueHeaderInner : Int -> ColorTheme a -> ElmValue -> Html msg
viewValueHeaderInner level colorTheme value =
    let
        viewValueFn =
            viewValueHeaderInner (level + 1) colorTheme
    in
    case value of
        Expandable _ (ElmSequence seqType children) ->
            let
                typeToString =
                    case seqType of
                        SeqSet ->
                            "Set"

                        SeqList ->
                            "List"

                        SeqArray ->
                            "Array"

                        SeqTuple ->
                            "Tuple"
            in
            case seqType of
                SeqTuple ->
                    if level > 1 then
                        Html.span []
                            [ Html.text "(…)"
                            ]

                    else
                        Html.span []
                            [ Html.text "("
                            , Html.span []
                                (List.map viewValueFn children
                                    |> List.intersperse (Html.text ", ")
                                )
                            , Html.text ")"
                            ]

                _ ->
                    Html.span []
                        [ Html.span [ Attrs.css [ Css.color colorTheme.sequenceNameColor ] ]
                            [ Html.text typeToString
                            ]
                        , Html.text <| "(" ++ String.fromInt (List.length children) ++ ")"
                        ]

        Expandable _ (ElmRecord recordValues) ->
            let
                keySpan name =
                    Html.span []
                        [ Html.span
                            [ Attrs.css
                                [ Css.color colorTheme.keysColor
                                , Css.fontStyle Css.italic
                                ]
                            ]
                            [ Html.text name ]
                        , Html.text ": "
                        ]
            in
            if level > 1 then
                List.head recordValues
                    |> Maybe.map
                        (\( name, _ ) ->
                            Html.span []
                                [ Html.text "{ "
                                , keySpan name
                                , Html.text "… }"
                                ]
                        )
                    |> Maybe.withDefault (Html.text "")

            else
                Html.span
                    []
                    (case recordValues of
                        [] ->
                            [ Html.text "{}" ]

                        [ ( name, singleValue ) ] ->
                            [ Html.text "{ "
                            , keySpan name
                            , viewValueFn singleValue
                            , Html.text " }"
                            ]

                        ( name, firstItem ) :: ( secName, secItem ) :: rest ->
                            [ Html.text "{ "
                            , keySpan name
                            , viewValueFn firstItem
                            , Html.text ", "
                            , keySpan secName
                            , viewValueFn secItem
                            , if List.isEmpty rest then
                                Html.text " }"

                              else
                                Html.text ", … }"
                            ]
                    )

        Expandable _ (ElmDict dictValues) ->
            Html.span []
                [ Html.span [ Attrs.css [ Css.color colorTheme.sequenceNameColor ] ]
                    [ Html.text "Dict"
                    ]
                , Html.text <| "(" ++ (String.fromInt <| List.length dictValues) ++ ")"
                ]

        Expandable _ (ElmType name values) ->
            let
                isTypeWithValues value_ =
                    case value_ of
                        Expandable _ (ElmType _ (_ :: _)) ->
                            True

                        _ ->
                            False
            in
            case values of
                [] ->
                    Html.span
                        [ Attrs.css [ Css.color colorTheme.customTypesColor ]
                        ]
                        [ Html.text name ]

                [ oneValue ] ->
                    Html.span []
                        [ Html.span
                            [ Attrs.css [ Css.color colorTheme.customTypesColor ]
                            ]
                            [ Html.text (name ++ " ") ]
                        , viewValueFn oneValue
                        ]

                first :: rest ->
                    Html.span []
                        ([ Html.span
                            [ Attrs.css [ Css.color colorTheme.customTypesColor ]
                            ]
                            [ Html.text name ]
                         , Html.text " "
                         ]
                            ++ (if level > 1 && (not <| List.isEmpty rest) then
                                    [ Html.text "(", viewValueFn first, Html.text " … )" ]

                                else if level > 0 then
                                    [ Html.text "(", viewValueFn first, Html.text " … )" ]

                                else
                                    values
                                        |> List.map
                                            (\v ->
                                                Html.span []
                                                    (if isTypeWithValues v then
                                                        [ Html.text "(", viewValueFn v, Html.text ")" ]

                                                     else
                                                        [ viewValueFn v ]
                                                    )
                                            )
                                        |> List.intersperse (Html.text " ")
                               )
                        )

        Plain (ElmString str) ->
            Html.span [ Attrs.css [ Css.color colorTheme.stringColor ] ] [ Html.text <| "\"" ++ str ++ "\"" ]

        Plain (ElmChar str) ->
            Html.span [ Attrs.css [ Css.color colorTheme.stringColor ] ] [ Html.text <| "'" ++ String.fromChar str ++ "'" ]

        Plain (ElmNumber float) ->
            Html.span [ Attrs.css [ Css.color colorTheme.numbersColor ] ] [ Html.text <| String.fromFloat float ]

        Plain (ElmBool bool) ->
            Html.span [ Attrs.css [ Css.color colorTheme.booleanColor ] ]
                [ Html.text <|
                    if bool then
                        "True"

                    else
                        "False"
                ]

        Plain ElmInternals ->
            Html.span [ Attrs.css [ Css.color colorTheme.internalsColor ] ]
                [ Html.text "<internals>" ]

        Plain ElmFunction ->
            Html.span [ Attrs.css [ Css.color colorTheme.internalsColor ] ]
                [ Html.text "<function>" ]

        Plain ElmUnit ->
            Html.text "()"

        Plain (ElmFile name) ->
            Html.text name

        Plain (ElmBytes count) ->
            Html.text <| String.fromInt count ++ "B"


triangle : ColorTheme a -> Bool -> Html msg
triangle colorTheme isOpened =
    Html.div
        [ Attrs.css
            [ Css.display Css.inlineBlock
            , Css.width (Css.px 12)
            , Css.color colorTheme.expandTriangleColor
            ]
        ]
        [ if isOpened then
            Html.text "▾"

          else
            Html.text "▸"
        ]


valueHeader : ColorTheme a -> (Path -> msg) -> Path -> Maybe (Html msg) -> ElmValue -> Html msg
valueHeader colorTheme toggleMsg toggleKey maybeKey value =
    let
        viewValueContent =
            if isValueExpanded value then
                Html.div [] [ viewValue colorTheme toggleMsg toggleKey value ]

            else
                Html.text ""

        headerValue =
            viewValueHeader colorTheme value

        headerWithKey =
            case maybeKey of
                Nothing ->
                    headerValue

                Just path ->
                    Html.span []
                        [ path
                        , Html.span [] [ Html.text ": " ]
                        , headerValue
                        ]
    in
    if ElmValue.hasNestedValues value then
        Html.div [] <|
            [ Html.span
                [ Attrs.fromUnstyled <| Events.onClickStopPropagation <| toggleMsg toggleKey
                , Attrs.css
                    [ Css.cursor Css.pointer
                    , Css.hover [ Css.backgroundColor <| colorTheme.valueBackgroundColor, Css.textDecoration Css.underline ]
                    ]
                ]
                [ triangle colorTheme (isValueExpanded value), headerWithKey ]
            , viewValueContent
            ]

    else
        Html.div [ Attrs.css [ Css.marginLeft <| Css.px 12 ] ] [ headerWithKey ]


viewValue : ColorTheme a -> (Path -> msg) -> Path -> ElmValue -> Html msg
viewValue colorTheme toggleMsg parentKey value =
    let
        toggleKey idx =
            parentKey ++ [ idx ]

        header idx =
            valueHeader colorTheme toggleMsg (toggleKey idx)

        childrenWrapper children =
            Html.div
                [ Attrs.css
                    [ Css.paddingLeft (Css.em 1)
                    , Css.marginLeft (Css.px 3)
                    , Css.borderLeft3 (Css.px 1) Css.solid colorTheme.guidelinesColor
                    ]
                ]
                children

        toggableDivWrapper idx child =
            header idx
                (Just <|
                    Html.span
                        [ Attrs.css
                            [ Css.color colorTheme.keysColor
                            , Css.fontStyle Css.italic
                            ]
                        ]
                        [ Html.text <| String.fromInt idx ]
                )
                child
    in
    case value of
        Expandable _ (ElmSequence _ children) ->
            children
                |> List.indexedMap toggableDivWrapper
                |> childrenWrapper

        Expandable _ (ElmRecord recordValues) ->
            recordValues
                |> List.indexedMap
                    (\idx ( key, child ) ->
                        header idx
                            (Just <|
                                Html.span
                                    [ Attrs.css
                                        [ Css.color colorTheme.keysColor
                                        , Css.fontStyle Css.italic
                                        ]
                                    ]
                                    [ Html.text key ]
                            )
                            child
                    )
                |> childrenWrapper

        Expandable _ (ElmDict dictValues) ->
            dictValues
                |> List.indexedMap
                    (\idx ( path, dictValue ) ->
                        header idx
                            (Just <|
                                viewValueHeader colorTheme path
                            )
                            dictValue
                    )
                |> childrenWrapper

        Expandable _ (ElmType name values) ->
            case values of
                [] ->
                    Html.span [ Attrs.css [ Css.color colorTheme.customTypesColor ] ] [ Html.text name ]

                children ->
                    children
                        |> List.indexedMap (\idx child -> header idx Nothing child)
                        |> childrenWrapper

        _ ->
            viewValueHeader colorTheme value

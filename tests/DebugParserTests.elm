module DebugParserTests exposing (fuzzSuite, suite)

import DebugParser
import DebugParser.ElmValue exposing (ElmValue(..), ExpandableValue(..), PlainValue(..), SequenceType(..))
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (..)


type alias RecursiveFuzzerConfig a =
    { maxDepth : Int
    , baseWeight : Float
    , recurseWeight : Int -> Float
    , base : Fuzzer a
    , recurse : Fuzzer a -> Fuzzer a
    }


recursiveFuzzer : RecursiveFuzzerConfig a -> Fuzzer a
recursiveFuzzer { maxDepth, baseWeight, recurseWeight, base, recurse } =
    let
        helper depth =
            if depth > maxDepth then
                base

            else
                Fuzz.frequency
                    [ ( baseWeight, base )
                    , ( recurseWeight depth, recurse (helper (depth + 1)) )
                    ]
    in
    helper 1


fuzzTag : Fuzzer String
fuzzTag =
    Fuzz.string
        |> Fuzz.map (String.trim >> String.replace ": " "_")


fuzzBool : Fuzzer ( String, ElmValue )
fuzzBool =
    Fuzz.bool
        |> Fuzz.map
            (\bool ->
                ( Debug.toString bool, Plain <| ElmBool bool )
            )


fuzzInt : Fuzzer ( String, ElmValue )
fuzzInt =
    Fuzz.int
        |> Fuzz.map
            (\int ->
                ( Debug.toString int, Plain <| ElmNumber (toFloat int) )
            )


fuzzFloat : Fuzzer ( String, ElmValue )
fuzzFloat =
    Fuzz.niceFloat
        |> Fuzz.map
            (\float ->
                ( Debug.toString float, Plain <| ElmNumber float )
            )


fuzzInfinity : Fuzzer ( String, ElmValue )
fuzzInfinity =
    Fuzz.oneOf
        [ Fuzz.constant ( "Infinity", Plain <| ElmNumber (1 / 0) )
        , Fuzz.constant ( "-Infinity", Plain <| ElmNumber -(1 / 0) )
        ]


fuzzNumber : Fuzzer ( String, ElmValue )
fuzzNumber =
    Fuzz.oneOf
        [ fuzzInt
        , fuzzFloat
        , fuzzInfinity
        ]


fuzzUnit : Fuzzer ( String, ElmValue )
fuzzUnit =
    Fuzz.constant ( "()", Plain ElmUnit )


fuzzInternals : Fuzzer ( String, ElmValue )
fuzzInternals =
    Fuzz.constant ( "<internals>", Plain ElmInternals )


fuzzFunction : Fuzzer ( String, ElmValue )
fuzzFunction =
    Fuzz.constant ( "<function>", Plain ElmFunction )


fuzzString : Fuzzer ( String, ElmValue )
fuzzString =
    Fuzz.string
        |> Fuzz.map (\str -> ( Debug.toString str, Plain <| ElmString str ))


fuzzChar : Fuzzer ( String, ElmValue )
fuzzChar =
    Fuzz.char
        |> Fuzz.map (\char -> ( Debug.toString char, Plain <| ElmChar char ))


fuzzCustomType : Fuzzer ( String, ElmValue )
fuzzCustomType =
    let
        buildCustomType ( name, _ ) list =
            List.unzip list
                |> Tuple.mapFirst (\strList -> name ++ " " ++ String.join " " strList |> String.trim)
                |> Tuple.mapSecond (Expandable False << ElmType name)
    in
    recursiveFuzzer
        { maxDepth = 3
        , baseWeight = 1
        , recurseWeight = always 0
        , base =
            Fuzz.map2 buildCustomType
                fuzzTypeWithoutValue
                (Fuzz.list fuzzCustomTypeValue)
        , recurse = \_ -> fuzzCustomTypeValue
        }


fuzzCustomTypeValue : Fuzzer ( String, ElmValue )
fuzzCustomTypeValue =
    Fuzz.oneOf
        [ fuzzBool
        , fuzzUnit
        , fuzzNumber
        , fuzzInternals
        , fuzzFunction
        , fuzzString
        , fuzzChar
        , fuzzTypeWithoutValue
        ]


fuzzValue : Fuzzer ( String, ElmValue )
fuzzValue =
    Fuzz.oneOf
        [ fuzzBool
        , fuzzUnit
        , fuzzNumber
        , fuzzInternals
        , fuzzFunction
        , fuzzString
        , fuzzChar
        , fuzzTypeWithoutValue
        , fuzzCustomType
        , fuzzListValue fuzzString
        , fuzzArrayValue fuzzNumber
        ]


fuzzTuple : Fuzzer ( String, ElmValue )
fuzzTuple =
    let
        buildTuple ( fstStr, fstVal ) ( sndStr, sndVal ) =
            ( "(" ++ fstStr ++ "," ++ sndStr ++ ")", Expandable False <| ElmSequence SeqTuple [ fstVal, sndVal ] )

        tupleValue next =
            Fuzz.oneOf
                [ fuzzValue
                , next
                ]
    in
    recursiveFuzzer
        { maxDepth = 3
        , baseWeight = 1
        , recurseWeight = \depth -> (1 / 5) / toFloat depth
        , base = Fuzz.map2 buildTuple fuzzValue fuzzValue
        , recurse =
            \next -> Fuzz.map2 buildTuple (tupleValue next) (tupleValue next)
        }


fuzzTriplet : Fuzzer ( String, ElmValue )
fuzzTriplet =
    let
        buildTriplet ( fstStr, fstVal ) ( sndStr, sndVal ) ( rdStr, rdVal ) =
            ( "(" ++ fstStr ++ "," ++ sndStr ++ "," ++ rdStr ++ ")", Expandable False <| ElmSequence SeqTuple [ fstVal, sndVal, rdVal ] )

        tripletValue next =
            Fuzz.oneOf [ fuzzValue, next, fuzzTuple ]
    in
    recursiveFuzzer
        { maxDepth = 3
        , baseWeight = 1
        , recurseWeight = \depth -> (1 / 5) / toFloat depth
        , base = Fuzz.map3 buildTriplet fuzzValue fuzzValue fuzzValue
        , recurse =
            \next ->
                Fuzz.map3 buildTriplet
                    (tripletValue next)
                    (tripletValue next)
                    (tripletValue next)
        }


fuzzTypeName : Fuzzer String
fuzzTypeName =
    let
        lowerCaseGroup =
            Fuzz.intRange 97 122
                |> Fuzz.map Char.fromCode

        upperCaseGroup =
            Fuzz.intRange 65 90
                |> Fuzz.map Char.fromCode

        numberGroup =
            Fuzz.intRange 48 57
                |> Fuzz.map Char.fromCode

        charsList =
            Fuzz.oneOf [ lowerCaseGroup, upperCaseGroup, numberGroup, Fuzz.constant '_' ]
                |> Fuzz.list
                |> Fuzz.map String.fromList
    in
    Fuzz.map2 String.cons upperCaseGroup charsList
        |> Fuzz.filter (\name -> not (List.member name [ "NaN", "Infinity" ]))


fuzzTypeWithoutValue : Fuzzer ( String, ElmValue )
fuzzTypeWithoutValue =
    fuzzTypeName
        |> Fuzz.map (\typeName -> ( typeName, Expandable False <| ElmType typeName [] ))


fuzzListValue : Fuzzer ( String, ElmValue ) -> Fuzzer ( String, ElmValue )
fuzzListValue valueFuzzer =
    Fuzz.list valueFuzzer
        |> Fuzz.map
            (List.foldl (\( str, v ) ( resultStr, values ) -> ( str :: resultStr, v :: values ))
                ( [], [] )
                >> (\( strList, valList ) ->
                        ( "[" ++ String.join "," strList ++ "]", Expandable False <| ElmSequence SeqList valList )
                   )
            )


fuzzArrayValue : Fuzzer ( String, ElmValue ) -> Fuzzer ( String, ElmValue )
fuzzArrayValue valueFuzzer =
    Fuzz.list valueFuzzer
        |> Fuzz.map
            (List.foldl (\( str, v ) ( resultStr, values ) -> ( str :: resultStr, v :: values ))
                ( [], [] )
                >> (\( strList, valList ) ->
                        ( "Array.fromList [" ++ String.join "," strList ++ "]", Expandable False <| ElmSequence SeqArray valList )
                   )
            )


fuzzSetValue : Fuzzer ( String, ElmValue ) -> Fuzzer ( String, ElmValue )
fuzzSetValue valueFuzzer =
    Fuzz.list valueFuzzer
        |> Fuzz.map
            (List.foldl (\( str, v ) ( resultStr, values ) -> ( str :: resultStr, v :: values ))
                ( [], [] )
                >> (\( strList, valList ) ->
                        ( "Set.fromList [" ++ String.join "," strList ++ "]", Expandable False <| ElmSequence SeqSet valList )
                   )
            )


fuzzSequencesValues : (Fuzzer ( String, ElmValue ) -> Fuzzer ( String, ElmValue )) -> Fuzzer ( String, ElmValue )
fuzzSequencesValues fn =
    Fuzz.oneOf
        [ fn fuzzBool
        , fn fuzzUnit
        , fn fuzzInt
        , fn fuzzFloat
        , fn fuzzInternals
        , fn fuzzFunction
        , fn fuzzTuple
        , fn fuzzTriplet
        , fn fuzzRecord

        -- type
        ]


fuzzVariableName : Fuzzer String
fuzzVariableName =
    let
        lowerCaseGroup =
            Fuzz.intRange 97 122
                |> Fuzz.map Char.fromCode

        upperCaseGroup =
            Fuzz.intRange 65 90
                |> Fuzz.map Char.fromCode

        numberGroup =
            Fuzz.intRange 48 57
                |> Fuzz.map Char.fromCode

        charsList =
            Fuzz.oneOf [ lowerCaseGroup, upperCaseGroup, numberGroup, Fuzz.constant '_' ]
                |> Fuzz.list
                |> Fuzz.map String.fromList
    in
    Fuzz.map2 String.cons lowerCaseGroup charsList


fuzzRecord : Fuzzer ( String, ElmValue )
fuzzRecord =
    let
        fuzzRecordList =
            Fuzz.list fuzzRecordEntry

        fuzzRecordEntry =
            Fuzz.pair fuzzVariableName fuzzValue
    in
    Fuzz.map2
        (\firstEntry list ->
            (firstEntry :: list)
                |> List.map
                    (\( varName, ( valString, value ) ) ->
                        ( varName ++ " = " ++ valString, ( varName, value ) )
                    )
                |> List.unzip
                |> Tuple.mapFirst (\strList -> "{ " ++ String.join ", " strList ++ " }")
                |> Tuple.mapSecond (Expandable False << ElmRecord)
        )
        fuzzRecordEntry
        fuzzRecordList



{--Dict --}


fuzzDict : Fuzzer ( String, ElmValue )
fuzzDict =
    let
        keyValue =
            Fuzz.oneOf [ fuzzBool, fuzzNumber, fuzzString, fuzzChar, fuzzTuple ]

        buildTuple ( fstStr, fstVal ) ( sndStr, sndVal ) =
            ( "(" ++ fstStr ++ "," ++ sndStr ++ ")", ( fstVal, sndVal ) )

        fuzzDictVal : Fuzzer ( String, ( ElmValue, ElmValue ) )
        fuzzDictVal =
            Fuzz.map2 buildTuple keyValue fuzzValue
    in
    Fuzz.list fuzzDictVal
        |> Fuzz.map
            (\list ->
                List.unzip list
                    |> Tuple.mapFirst (\strList -> "Dict.fromList [" ++ String.join ", " strList ++ "]")
                    |> Tuple.mapSecond (Expandable False << ElmDict)
            )


suite : Test
suite =
    describe "Parse" <|
        [ test "Simple bolean value"
            (\_ ->
                Expect.equal (DebugParser.parse "Debug: True")
                    (Ok { tag = "Debug", value = Plain <| ElmBool True })
            )
        , test "Tuple"
            (\_ ->
                Expect.equal (DebugParser.parse "Debug: (True,False)")
                    (Ok { tag = "Debug", value = Expandable False <| ElmSequence SeqTuple [ Plain <| ElmBool True, Plain <| ElmBool False ] })
            )
        , test "Nested Tuple"
            (\_ ->
                Expect.equal (DebugParser.parse "Debug: (True,(2,()))")
                    (Ok
                        { tag = "Debug"
                        , value =
                            Expandable False <|
                                ElmSequence SeqTuple
                                    [ Plain <| ElmBool True
                                    , Expandable False <|
                                        ElmSequence SeqTuple [ Plain <| ElmNumber 2, Plain ElmUnit ]
                                    ]
                        }
                    )
            )
        , test "Empty list"
            (\_ ->
                Expect.equal (DebugParser.parse "Debug: []")
                    (Ok { tag = "Debug", value = Expandable False <| ElmSequence SeqList [] })
            )
        , test "Parse bytes"
            (\_ ->
                "msg: <123 bytes>"
                    |> DebugParser.parse
                    |> Result.map .value
                    |> Expect.equal (Ok (Plain <| ElmBytes 123))
            )
        , test "Parse file"
            (\_ ->
                "msg: <filename>"
                    |> DebugParser.parse
                    |> Result.map .value
                    |> Expect.equal (Ok (Plain <| ElmFile "filename"))
            )
        , test "Parse NaN"
            (\_ ->
                DebugParser.parse "Debug: NaN"
                    |> Result.map
                        (\parsed ->
                            case parsed.value of
                                Plain (ElmNumber a) ->
                                    isNaN a

                                _ ->
                                    False
                        )
                    |> Expect.equal (Ok True)
            )
        , test "Parse string without closing quote fails"
            (\_ ->
                Expect.err (DebugParser.parse "Debug: \"Debug message")
            )
        , test "Parse string with emoji"
            (\_ ->
                Expect.equal (DebugParser.parse "Debug: \"Debug message👨\u{200D}💻\"")
                    (Ok { tag = "Debug", value = Plain <| ElmString "Debug message👨\u{200D}💻" })
            )
        , test "Parse nonstandard chars "
            (\_ ->
                Expect.equal (DebugParser.parse "Debug: ['\u{000D}', '👨', '\\'', '\n' ]")
                    (Ok
                        { tag = "Debug"
                        , value =
                            Expandable False <|
                                ElmSequence SeqList
                                    [ Plain <| ElmChar '\u{000D}'
                                    , Plain <| ElmChar '👨'
                                    , Plain <| ElmChar '\''
                                    , Plain <| ElmChar '\n'
                                    ]
                        }
                    )
            )
        , describe "Multiple types"
            [ test "Just boolean"
                (\_ ->
                    Expect.equal (DebugParser.parse "Debug: Just True")
                        (Ok { tag = "Debug", value = Expandable False <| ElmType "Just" [ Plain <| ElmBool True ] })
                )
            , test "Multiple values "
                (\_ ->
                    Expect.equal (DebugParser.parse "Debug: Just True Nothing")
                        (Ok { tag = "Debug", value = Expandable False <| ElmType "Just" [ Plain <| ElmBool True, Expandable False <| ElmType "Nothing" [] ] })
                )
            , test "Multiple values with parentheses"
                (\_ ->
                    Expect.equal (DebugParser.parse "Debug: Just (Just True)")
                        (Ok { tag = "Debug", value = Expandable False <| ElmType "Just" [ Expandable False <| ElmType "Just" [ Plain <| ElmBool True ] ] })
                )
            , test "Multiple values without parentheses"
                (\_ ->
                    Expect.equal (DebugParser.parse "Debug: A A ()")
                        (Ok { tag = "Debug", value = Expandable False <| ElmType "A" [ Expandable False <| ElmType "A" [], Plain <| ElmUnit ] })
                )
            , test "Multiple values with different types"
                (\_ ->
                    Expect.equal (DebugParser.parse "Debug: Custom (Just True 12 \"string\" Nothing) Infinity -Infinity")
                        (Ok
                            { tag = "Debug"
                            , value =
                                Expandable False <|
                                    ElmType "Custom"
                                        [ Expandable False <|
                                            ElmType
                                                "Just"
                                                [ Plain <| ElmBool True
                                                , Plain <| ElmNumber 12
                                                , Plain <| ElmString "string"
                                                , Expandable False <| ElmType "Nothing" []
                                                ]
                                        , Plain <| ElmNumber (1 / 0)
                                        , Plain <| ElmNumber -(1 / 0)
                                        ]
                            }
                        )
                )
            ]
        , test "Custom type at the end of record"
            (\_ ->
                Expect.equal (DebugParser.parse "Debug: { a = A }")
                    (Ok { tag = "Debug", value = Expandable False <| ElmRecord [ ( "a", Expandable False <| ElmType "A" [] ) ] })
            )
        , test "CustomType within custom type"
            (\_ ->
                "msg: StoreMsg (Fetched { copiedFrom = Nothing })"
                    |> DebugParser.parse
                    |> Expect.equal
                        (Ok
                            { tag = "msg"
                            , value =
                                Expandable False <|
                                    ElmType
                                        "StoreMsg"
                                        [ Expandable False <|
                                            ElmType
                                                "Fetched"
                                                [ Expandable False <| ElmRecord [ ( "copiedFrom", Expandable False <| ElmType "Nothing" [] ) ]
                                                ]
                                        ]
                            }
                        )
            )
        , test "Custom type with Dict value"
            (\_ ->
                "msg: Leaf \"A\" (Dict.fromList [])"
                    |> DebugParser.parse
                    |> Expect.equal
                        (Ok
                            { tag = "msg"
                            , value =
                                Expandable False <|
                                    ElmType "Leaf" [ Plain <| ElmString "A", Expandable False <| ElmDict [] ]
                            }
                        )
            )
        , test "problematic group"
            (\_ ->
                "msg: Group (Dict.fromList [(\"city\",Value (String \"\"))])"
                    |> DebugParser.parse
                    |> Expect.equal
                        (Ok
                            { tag = "msg"
                            , value =
                                Expandable False <|
                                    ElmType "Group"
                                        [ Expandable False <|
                                            ElmDict
                                                [ ( Plain <| ElmString "city"
                                                  , Expandable False <|
                                                        ElmType "Value"
                                                            [ Expandable False <|
                                                                ElmType "String"
                                                                    [ Plain <| ElmString ""
                                                                    ]
                                                            ]
                                                  )
                                                ]
                                        ]
                            }
                        )
            )
        , test "real world example is parsed"
            (\_ ->
                "Debug with 2 numbers 7 chars like !_+))($ and emojis 💪 : { array = Array.fromList [1,2,3,4,5678,3464637,893145,-29], bools = (True,False), complexTuple = (1,(\"longer string\",(\"much longer string\",1))), dict = Dict.fromList [(1,\"a\"),(2,\"b\"),(234,\"String longer than one char\")], dictWithTuples = Dict.fromList [((0,\"b\",1),\"a\"),((0,\"c\",1),\"b\"),((4,\"d\",1),\"String longer than one char\")], float = 123.56, function = <function>, int = 123, listOfLists = [[[\"a\",\"b\"],[\"c\",\"d\"]],[[\"e\",\"f\"],[\"g\",\"h\"]]], listSingleton = [\"Singleton\"], nonEmptyList = (1,[]), set = Set.fromList [\"Some really long string with some nonsense\",\"a\",\"b\"], string = \"Some string\", triplet = (1,\"b\",1), tuple = (1,2), unit = (), test = A { custom = B } }"
                    |> DebugParser.parse
                    |> Expect.ok
            )
        , test "Parse char with two backslashes "
            (\_ ->
                Expect.equal (DebugParser.parse "Debug: ['\\t' ]")
                    (Ok
                        { tag = "Debug"
                        , value =
                            Expandable False <|
                                ElmSequence SeqList [ Plain <| ElmChar '\t' ]
                        }
                    )
            )
        ]


fuzzSuite : Test
fuzzSuite =
    let
        checkParsing ( str, val ) =
            DebugParser.parse (": " ++ str)
                |> Result.map .value
                |> Expect.equal (Ok val)
    in
    describe "Fuzz tests"
        [ fuzz fuzzTag
            "Tag can be any string except string contains `: `"
            (\tag ->
                Expect.equal (DebugParser.parse (tag ++ ": True"))
                    (Ok { tag = tag, value = Plain <| ElmBool True })
            )
        , fuzz fuzzValue
            "Base value is parsed without problems"
            checkParsing
        , fuzz fuzzTuple
            "Tuple is parsed without problems"
            checkParsing
        , fuzz fuzzTriplet
            "Triplet is parsed without problems"
            checkParsing
        , fuzz (fuzzSequencesValues fuzzSetValue)
            "Set is parsed without problems"
            checkParsing
        , fuzz fuzzRecord
            "Record is parsed without problems"
            checkParsing
        , fuzz fuzzDict
            "Simple Dict is parsed without problems"
            checkParsing
        , fuzz fuzzCustomType
            "Custom types"
            checkParsing
        ]

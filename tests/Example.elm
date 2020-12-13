module Example exposing (..)

import DebugParser
import Dict exposing (Dict)
import Expandable exposing (ElmValue(..), SequenceType(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
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
                ( Debug.toString bool, ElmBool bool )
            )


fuzzInt : Fuzzer ( String, ElmValue )
fuzzInt =
    Fuzz.int
        |> Fuzz.map
            (\int ->
                ( Debug.toString int, ElmInt int )
            )


fuzzFloat : Fuzzer ( String, ElmValue )
fuzzFloat =
    Fuzz.float
        |> Fuzz.map
            (\float ->
                if float == (round float |> toFloat) then
                    ( Debug.toString float, ElmInt (round float) )

                else
                    ( Debug.toString float, ElmFloat float )
            )


fuzzInfinity : Fuzzer ( String, ElmValue )
fuzzInfinity =
    Fuzz.oneOf
        [ Fuzz.constant ( "Infinity", ElmFloat (1 / 0) )
        , Fuzz.constant ( "-Infinity", ElmFloat -(1 / 0) )
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
    Fuzz.constant ( "()", ElmUnit )


fuzzInternals : Fuzzer ( String, ElmValue )
fuzzInternals =
    Fuzz.constant ( "<internals>", ElmInternals )


fuzzFunction : Fuzzer ( String, ElmValue )
fuzzFunction =
    Fuzz.constant ( "<function>", ElmFunction )


fuzzString : Fuzzer ( String, ElmValue )
fuzzString =
    Fuzz.string
        |> Fuzz.map (\str -> ( Debug.toString str, ElmString str ))


fuzzChar : Fuzzer ( String, ElmValue )
fuzzChar =
    Fuzz.char
        |> Fuzz.map (\char -> ( Debug.toString char, ElmChar char ))


fuzzCustomType : Fuzzer ( String, ElmValue )
fuzzCustomType =
    let
        buildCustomType ( name, _ ) list =
            List.unzip list
                |> Tuple.mapFirst (\strList -> name ++ " " ++ String.join " " strList |> String.trim)
                |> Tuple.mapSecond (ElmType False name)
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
        ]


fuzzTuple : Fuzzer ( String, ElmValue )
fuzzTuple =
    let
        buildTuple ( fstStr, fstVal ) ( sndStr, sndVal ) =
            ( "(" ++ fstStr ++ "," ++ sndStr ++ ")", ElmTuple False [ fstVal, sndVal ] )

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
            ( "(" ++ fstStr ++ "," ++ sndStr ++ "," ++ rdStr ++ ")", ElmTuple False [ fstVal, sndVal, rdVal ] )

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


fuzzTypeWithoutValue : Fuzzer ( String, ElmValue )
fuzzTypeWithoutValue =
    fuzzTypeName
        |> Fuzz.map (\typeName -> ( typeName, ElmType False typeName [] ))


fuzzListValue : Fuzzer ( String, ElmValue ) -> Fuzzer ( String, ElmValue )
fuzzListValue valueFuzzer =
    Fuzz.list valueFuzzer
        |> Fuzz.map
            (List.foldl (\( str, v ) ( resultStr, values ) -> ( str :: resultStr, v :: values ))
                ( [], [] )
                >> (\( strList, valList ) ->
                        ( "[" ++ String.join "," strList ++ "]", ElmSequence List False valList )
                   )
            )


fuzzArrayValue : Fuzzer ( String, ElmValue ) -> Fuzzer ( String, ElmValue )
fuzzArrayValue valueFuzzer =
    Fuzz.list valueFuzzer
        |> Fuzz.map
            (List.foldl (\( str, v ) ( resultStr, values ) -> ( str :: resultStr, v :: values ))
                ( [], [] )
                >> (\( strList, valList ) ->
                        ( "Array.fromList [" ++ String.join "," strList ++ "]", ElmSequence Array False valList )
                   )
            )


fuzzSetValue : Fuzzer ( String, ElmValue ) -> Fuzzer ( String, ElmValue )
fuzzSetValue valueFuzzer =
    Fuzz.list valueFuzzer
        |> Fuzz.map
            (List.foldl (\( str, v ) ( resultStr, values ) -> ( str :: resultStr, v :: values ))
                ( [], [] )
                >> (\( strList, valList ) ->
                        ( "Set.fromList [" ++ String.join "," strList ++ "]", ElmSequence Set False valList )
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
            Fuzz.tuple ( fuzzVariableName, fuzzValue )
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
                |> Tuple.mapSecond (ElmRecord False)
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
                    |> Tuple.mapSecond (ElmDict False)
            )



{- TODO:
   - File
   - Bytes
   - Custom Type w/ values and parens
-}


suite : Test
suite =
    describe "Parse"
        [ test "Simple bolean value"
            (\_ ->
                Expect.equal (DebugParser.parse "Debug: True")
                    (Ok { tag = "Debug", value = ElmBool True })
            )
        , test "Tuple"
            (\_ ->
                Expect.equal (DebugParser.parse "Debug: (True,False)")
                    (Ok { tag = "Debug", value = ElmTuple False [ ElmBool True, ElmBool False ] })
            )
        , test "Nested Tuple"
            (\_ ->
                Expect.equal (DebugParser.parse "Debug: (True,(2,()))")
                    (Ok { tag = "Debug", value = ElmTuple False [ ElmBool True, ElmTuple False [ ElmInt 2, ElmUnit ] ] })
            )
        , test "Empty list"
            (\_ ->
                Expect.equal (DebugParser.parse "Debug: []")
                    (Ok { tag = "Debug", value = ElmSequence List False [] })
            )
        , test "Parse NaN"
            (\_ ->
                DebugParser.parse "Debug: NaN"
                    |> Result.map
                        (\parsed ->
                            case parsed.value of
                                ElmFloat a ->
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
                    (Ok { tag = "Debug", value = ElmString "Debug message👨\u{200D}💻" })
            )
        , test "Parse nonstandard chars "
            (\_ ->
                Expect.equal (DebugParser.parse "Debug: ['\u{000D}', '👨', '\\'', '\n' ]")
                    (Ok { tag = "Debug", value = ElmSequence List False [ ElmChar '\u{000D}', ElmChar '👨', ElmChar '\'', ElmChar '\n' ] })
            )
        , describe "Multiple types"
            [ test "Just boolean"
                (\_ ->
                    Expect.equal (DebugParser.parse "Debug: Just True")
                        (Ok { tag = "Debug", value = ElmType False "Just" [ ElmBool True ] })
                )
            , test "Multiple values "
                (\_ ->
                    Expect.equal (DebugParser.parse "Debug: Just True Nothing")
                        (Ok { tag = "Debug", value = ElmType False "Just" [ ElmBool True, ElmType False "Nothing" [] ] })
                )
            , test "Multiple values with parentheses"
                (\_ ->
                    Expect.equal (DebugParser.parse "Debug: Just (Just True)")
                        (Ok { tag = "Debug", value = ElmType False "Just" [ ElmType False "Just" [ ElmBool True ] ] })
                )
            , test "Multiple values with different types"
                (\_ ->
                    Expect.equal (DebugParser.parse "Debug: Custom (Just True 12 \"string\" Nothing) Infinity -Infinity")
                        (Ok
                            { tag = "Debug"
                            , value =
                                ElmType False
                                    "Custom"
                                    [ ElmType False
                                        "Just"
                                        [ ElmBool True
                                        , ElmInt 12
                                        , ElmString "string"
                                        , ElmType False "Nothing" []
                                        ]
                                    , ElmFloat (1 / 0)
                                    , ElmFloat -(1 / 0)
                                    ]
                            }
                        )
                )
            ]
        , test "CustomType within custom type"
            (\_ ->
                "msg: CrosstabBuilderStoreMsg (XBProjectsFetched { copiedFrom = Nothing })"
                    |> DebugParser.parse
                    |> Expect.equal
                        (Ok
                            { tag = "msg"
                            , value =
                                ElmType False
                                    "CrosstabBuilderStoreMsg"
                                    [ ElmType False
                                        "XBProjectsFetched"
                                        [ ElmRecord False [ ( "copiedFrom", ElmType False "Nothing" [] ) ]
                                        ]
                                    ]
                            }
                        )
            )
        , test "Custom type with Dict value"
            (\_ ->
                "msg: Leaf \"A\" (Dict.fromList [])"
                    |> DebugParser.parse
                    |> Expect.equal (Ok { tag = "msg", value = ElmType False "Leaf" [ ElmString "A", ElmDict False [] ] })
            )
        , test "problematic group"
            (\_ ->
                "msg: Group (Dict.fromList [(\"city\",Value (String \"\"))])"
                    |> DebugParser.parse
                    |> Expect.equal
                        (Ok
                            { tag = "msg"
                            , value =
                                ElmType False
                                    "Group"
                                    [ ElmDict False [ ( ElmString "city", ElmType False "Value" [ ElmType False "String" [ ElmString "" ] ] ) ]
                                    ]
                            }
                        )
            )
        ]


type Tree
    = Leaf String (Dict.Dict Int String)
    | Node Tree Tree


fuzzSuite : Test
fuzzSuite =
    let
        debug ( str, val ) =
            ( Debug.log "" str, val )

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
                    (Ok { tag = tag, value = ElmBool True })
            )
        , only <|
            fuzz fuzzValue
                "Base value is parsed without problems"
                checkParsing
        , fuzz fuzzTuple
            "Tuple is parsed without problems"
            checkParsing
        , fuzz fuzzTriplet
            "Triplet is parsed without problems"
            checkParsing
        , fuzz (fuzzSequencesValues fuzzListValue)
            "List is parsed without problems"
            checkParsing
        , fuzz (fuzzSequencesValues fuzzArrayValue)
            "Array is parsed without problems"
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
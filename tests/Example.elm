module Example exposing (..)

import DebugParser
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
        |> Fuzz.map (String.replace ": " "_")


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


fuzzValue : Fuzzer ( String, ElmValue )
fuzzValue =
    Fuzz.oneOf
        [ fuzzBool
        , fuzzUnit
        , fuzzNumber
        , fuzzInternals
        , fuzzFunction
        , fuzzString
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
        ]



{- TODO:
   - Chars
   - File
   - Bytes
   - Dict
   - Record
   - Type/Custom Type
-}


suite : Test
suite =
    describe "Parse"
        [ test "Simple bolean value"
            (\_ ->
                Expect.equal (DebugParser.parse "Debug: True")
                    (Ok { tag = "Debug", value = ElmBool True })
            )
        , test "Nested Tuple"
            (\_ ->
                Expect.equal (DebugParser.parse "Debug: (True, (2, ()))")
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
        , test "Parse string with emoji"
            (\_ ->
                Expect.equal (DebugParser.parse "Debug: \"Debug message👨\u{200D}💻\"")
                    (Ok { tag = "Debug", value = ElmString "Debug message👨\u{200D}💻" })
            )
        ]


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
        , fuzz fuzzValue
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
        ]

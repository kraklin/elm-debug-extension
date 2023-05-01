module DebugParser exposing
    ( ParsedLog
    , parse
    , parseWithOptionalTag
    )

import DebugParser.ElmValue exposing (ElmValue(..), ExpandableValue(..), PlainValue(..), SequenceType(..))
import Parser as P exposing ((|.), (|=), DeadEnd, Parser, Step(..))


type alias ParsedLog =
    { tag : String
    , value : ElmValue
    }


{-| dead ends to string function is still not implemented in Parser library, so I had to copy paste this from another PR. :(
-}
deadEndsToString : List P.DeadEnd -> String
deadEndsToString deadEnds =
    let
        deadEndToString : DeadEnd -> String
        deadEndToString deadEnd =
            let
                position : String
                position =
                    "row:" ++ String.fromInt deadEnd.row ++ " col:" ++ String.fromInt deadEnd.col ++ "\n"
            in
            case deadEnd.problem of
                P.Expecting str ->
                    "Expecting '" ++ str ++ "' at " ++ position

                P.ExpectingInt ->
                    "ExpectingInt at " ++ position

                P.ExpectingHex ->
                    "ExpectingHex at " ++ position

                P.ExpectingOctal ->
                    "ExpectingOctal at " ++ position

                P.ExpectingBinary ->
                    "ExpectingBinary at " ++ position

                P.ExpectingFloat ->
                    "ExpectingFloat at " ++ position

                P.ExpectingNumber ->
                    "ExpectingNumber at " ++ position

                P.ExpectingVariable ->
                    "ExpectingVariable at " ++ position

                P.ExpectingSymbol str ->
                    "ExpectingSymbol '" ++ str ++ "' at " ++ position

                P.ExpectingKeyword str ->
                    "ExpectingKeyword '" ++ str ++ "' at " ++ position

                P.ExpectingEnd ->
                    "ExpectingEnd at " ++ position

                P.UnexpectedChar ->
                    "UnexpectedChar at " ++ position

                P.Problem str ->
                    "ProblemString '" ++ str ++ "' at " ++ position

                P.BadRepeat ->
                    "BadRepeat at " ++ position
    in
    List.foldl (\str acc -> acc ++ "\n" ++ str) "" (List.map deadEndToString deadEnds)


parseVariableName : Parser String
parseVariableName =
    P.getChompedString <|
        P.succeed ()
            |. P.chompIf Char.isLower
            |. P.chompWhile (\c -> Char.isAlphaNum c || c == '_')


parseTypeName : Parser String
parseTypeName =
    P.getChompedString <|
        P.succeed ()
            |. P.chompIf Char.isUpper
            |. P.chompWhile (\c -> Char.isAlphaNum c || c == '_')


parseNumber : Parser ElmValue
parseNumber =
    let
        number =
            P.chompWhile (\char -> Char.isDigit char || char == '.' || char == '+' || char == 'e' || char == '-')
                |> P.getChompedString
                |> P.andThen
                    (\str ->
                        case String.toFloat str of
                            Just float ->
                                P.succeed (ElmNumber float)

                            Nothing ->
                                P.problem "Unable to parse number"
                    )

        negateNumber value =
            case value of
                ElmNumber float ->
                    ElmNumber (negate float)

                _ ->
                    value
    in
    P.oneOf
        [ P.succeed (ElmNumber (0 / 0))
            |. P.keyword "NaN"
        , P.succeed (ElmNumber (1 / 0))
            |. P.keyword "Infinity"
        , P.succeed (ElmNumber -(1 / 0))
            |. P.keyword "-Infinity"
        , P.oneOf
            [ P.succeed negateNumber
                |. P.symbol "-"
                |= number
            , number
            ]
        ]
        |> P.map Plain


parseKeywords : Parser ElmValue
parseKeywords =
    P.oneOf
        [ P.succeed ElmInternals
            |. P.keyword "<internals>"
        , P.succeed ElmFunction
            |. P.keyword "<function>"
        ]
        |> P.map Plain


parseList : Parser ElmValue
parseList =
    P.sequence
        { start = "["
        , end = "]"
        , separator = ","
        , spaces = P.spaces
        , item = P.lazy (\_ -> parseValue)
        , trailing = P.Forbidden
        }
        |> P.map
            (\listVal ->
                Expandable False <| ElmSequence SeqList listVal
            )


parseArray : Parser ElmValue
parseArray =
    P.sequence
        { start = "Array.fromList ["
        , end = "]"
        , separator = ","
        , spaces = P.spaces
        , item = P.lazy (\_ -> parseValue)
        , trailing = P.Forbidden
        }
        |> P.map
            (\listVal ->
                Expandable False <| ElmSequence SeqArray listVal
            )


parseSet : Parser ElmValue
parseSet =
    P.sequence
        { start = "Set.fromList ["
        , end = "]"
        , separator = ","
        , spaces = P.spaces
        , item = P.lazy (\_ -> parseValue)
        , trailing = P.Forbidden
        }
        |> P.map
            (\listVal ->
                Expandable False <| ElmSequence SeqSet listVal
            )



{- ---- String parser ------ -}


parseString : Parser ElmValue
parseString =
    P.succeed identity
        |. P.token "\""
        |= P.loop [] (stringHelp "\"" isUninteresting)
        |> P.andThen
            (Maybe.map (\str -> P.succeed (Plain <| ElmString str))
                >> Maybe.withDefault (P.problem "One string has no closing double quotes")
            )


stringHelp : String -> (Char -> Bool) -> List String -> Parser (Step (List String) (Maybe String))
stringHelp endString charCheckFn revChunks =
    P.oneOf
        [ P.succeed (\chunk -> Loop (chunk :: revChunks))
            |. P.token "\\"
            |= P.oneOf
                [ P.map (\_ -> "\n") (P.token "n")
                , P.map (\_ -> "\t") (P.token "t")
                , P.map (\_ -> "\u{000D}") (P.token "r")
                , P.map (\_ -> "\u{000B}") (P.token "v")
                , P.map (\_ -> "\u{0000}") (P.token "0")
                , P.map (\_ -> "\\") (P.token "\\")
                , P.map (\_ -> "\"") (P.token "\"")
                , P.succeed String.fromChar
                    |. P.token "u{"
                    |= unicode
                    |. P.token "}"
                ]
        , P.oneOf
            [ P.token endString
                |> P.map (\_ -> Done (Just <| String.concat (List.reverse revChunks)))
            , P.end
                |> P.map (\_ -> Done Nothing)
            ]
        , P.chompWhile charCheckFn
            |> P.getChompedString
            |> P.map
                (\chunk ->
                    Loop (chunk :: revChunks)
                )
        ]


parseFile : Parser ElmValue
parseFile =
    P.succeed identity
        |. P.token "<"
        |= P.loop [] (stringHelp ">" (\c -> c /= '>'))
        |> P.andThen
            (Maybe.map (\str -> P.succeed (Plain <| ElmFile str))
                >> Maybe.withDefault (P.problem "File has no closing bracket")
            )


isUninteresting : Char -> Bool
isUninteresting char =
    char /= '\\' && char /= '"'


unicode : Parser Char
unicode =
    P.getChompedString (P.chompWhile Char.isHexDigit)
        |> P.andThen codeToChar


codeToChar : String -> Parser Char
codeToChar str =
    let
        length =
            String.length str

        code =
            String.foldl addHex 0 str
    in
    if 4 <= length && length <= 6 then
        P.problem "code point must have between 4 and 6 digits"

    else if 0 <= code && code <= 0x0010FFFF then
        P.succeed (Char.fromCode code)

    else
        P.problem "code point must be between 0 and 0x10FFFF"


addHex : Char -> Int -> Int
addHex char total =
    let
        code =
            Char.toCode char
    in
    if 0x30 <= code && code <= 0x39 then
        16 * total + (code - 0x30)

    else if 0x41 <= code && code <= 0x46 then
        16 * total + (10 + code - 0x41)

    else
        16 * total + (10 + code - 0x61)



{--Char parser --}


parseChar : Parser ElmValue
parseChar =
    P.oneOf
        [ P.succeed identity
            |. P.token "'\\''"
            |> P.map (\_ -> Plain <| ElmChar '\'')
        , P.succeed identity
            |. P.token "'\\t'"
            |> P.map (\_ -> Plain <| ElmChar '\t')
        , P.succeed identity
            |. P.token "'\\n'"
            |> P.map (\_ -> Plain <| ElmChar '\n')
        , P.succeed identity
            |. P.token "'\\v'"
            |> P.map (\_ -> Plain <| ElmChar '\u{000B}')
        , P.succeed identity
            |. P.token "'\\r'"
            |> P.map (\_ -> Plain <| ElmChar '\u{000D}')
        , P.succeed identity
            |. P.token "'\\0'"
            |> P.map (\_ -> Plain <| ElmChar '\u{0000}')
        , P.succeed identity
            |. P.token "'"
            |= P.getChompedString (P.chompUntil "'")
            |. P.token "'"
            |> P.map
                (String.toList
                    >> List.reverse
                    >> List.head
                    >> Maybe.withDefault 'x'
                    >> ElmChar
                    >> Plain
                )
        ]



{--Record parser --}


parseTypeWithoutValue : Parser ElmValue
parseTypeWithoutValue =
    parseTypeName
        |> P.map
            (\name ->
                case name of
                    "True" ->
                        Plain <| ElmBool True

                    "False" ->
                        Plain <| ElmBool False

                    "NaN" ->
                        Plain <| ElmNumber (0 / 0)

                    "Infinity" ->
                        Plain <| ElmNumber (1 / 0)

                    _ ->
                        Expandable False <| ElmType name []
            )


parseRecord : Parser ElmValue
parseRecord =
    P.sequence
        { start = "{"
        , end = "}"
        , separator = ","
        , spaces = P.spaces
        , item =
            P.lazy
                (\_ ->
                    P.succeed Tuple.pair
                        |= parseVariableName
                        |. P.token " = "
                        |= parseValue
                )
        , trailing = P.Forbidden
        }
        |> P.map
            (\listVal ->
                Expandable False <| ElmRecord listVal
            )


parseBytes : Parser ElmValue
parseBytes =
    -- TODO: the backtrackable can be removed with the combination of file parser
    P.backtrackable <|
        P.succeed (Plain << ElmBytes)
            |. P.token "<"
            |= P.int
            |. P.token " bytes>"



{--Custom type parser --}


parseCustomTypeWithoutValue : Parser ElmValue
parseCustomTypeWithoutValue =
    P.succeed
        (\name ->
            case name of
                "True" ->
                    Plain <| ElmBool True

                "False" ->
                    Plain <| ElmBool False

                "NaN" ->
                    Plain <| ElmNumber (0 / 0)

                "Infinity" ->
                    Plain <| ElmNumber (1 / 0)

                _ ->
                    --NOTE: This is actually not expandable at all. Maybe a tip for a refactoring it later
                    Expandable False <| ElmType name []
        )
        |= parseTypeName


parseCustomType : Parser ElmValue
parseCustomType =
    parseTypeName
        |> P.andThen
            (\name ->
                case name of
                    "True" ->
                        P.succeed (Plain <| ElmBool True)

                    "False" ->
                        P.succeed (Plain <| ElmBool False)

                    "NaN" ->
                        P.succeed (Plain <| ElmNumber (0 / 0))

                    "Infinity" ->
                        P.succeed (Plain <| ElmNumber (1 / 0))

                    _ ->
                        P.succeed
                            (\list ->
                                Expandable False <| ElmType name (List.reverse list)
                            )
                            |= P.loop [] typeHelp
            )


typeHelp : List ElmValue -> Parser (Step (List ElmValue) (List ElmValue))
typeHelp values =
    P.oneOf
        [ P.backtrackable <|
            P.succeed (\value -> Loop (value :: values))
                |. P.token " "
                |= parseValueWithoutCustomType
        , P.succeed (Done values)
        ]


parseValueWithParenthesis : Parser ElmValue
parseValueWithParenthesis =
    P.succeed identity
        |. P.token "("
        |= P.oneOf
            [ P.succeed identity
                |. P.spaces
                |= P.lazy (\_ -> parseValue)
                |> P.andThen
                    (\fstValue ->
                        P.oneOf
                            [ P.succeed identity
                                |. P.spaces
                                |. P.token ","
                                |. P.spaces
                                |= P.lazy (\_ -> parseValue)
                                |> P.andThen
                                    (\sndValue ->
                                        P.succeed identity
                                            |= P.oneOf
                                                [ -- ("x", "y", "z")
                                                  P.succeed identity
                                                    |. P.spaces
                                                    |. P.token ","
                                                    |. P.spaces
                                                    |= P.lazy (\_ -> parseValue)
                                                    |> P.map
                                                        (\rdValue ->
                                                            Expandable False <| ElmSequence SeqTuple [ fstValue, sndValue, rdValue ]
                                                        )
                                                , -- ("x", "y")
                                                  P.succeed
                                                    (Expandable False <| ElmSequence SeqTuple [ fstValue, sndValue ])
                                                ]
                                    )
                            , P.succeed fstValue
                            ]
                    )
            , P.succeed <| Plain ElmUnit
            ]
        |. P.token ")"



{--Dict parser-}


parseDict : Parser ElmValue
parseDict =
    P.sequence
        { start = "Dict.fromList ["
        , end = "]"
        , separator = ","
        , spaces = P.spaces
        , item =
            P.lazy
                (\_ ->
                    P.succeed Tuple.pair
                        |. P.token "("
                        |. P.spaces
                        |= P.lazy (\_ -> parseValue)
                        |. P.spaces
                        |. P.token ","
                        |. P.spaces
                        |= parseValue
                        |. P.spaces
                        |. P.token ")"
                )
        , trailing = P.Forbidden
        }
        |> P.map
            (\listVal ->
                Expandable False <| ElmDict listVal
            )



{- Main value parser -}


parseUnit : Parser ElmValue
parseUnit =
    P.succeed (Plain ElmUnit)
        |. P.keyword "()"


parseValueWithoutCustomType : Parser ElmValue
parseValueWithoutCustomType =
    P.oneOf
        [ parseRecord
        , parseArray
        , parseSet
        , parseDict
        , parseList
        , parseKeywords
        , parseCustomTypeWithoutValue
        , parseNumber
        , parseValueWithParenthesis
        , parseChar
        , parseString
        , parseBytes
        , parseFile
        ]


parseValue : Parser ElmValue
parseValue =
    P.oneOf
        [ parseRecord
        , parseArray
        , parseSet
        , parseDict
        , parseList
        , parseKeywords
        , P.lazy (\_ -> parseCustomType)
        , parseCustomTypeWithoutValue
        , parseNumber
        , parseValueWithParenthesis
        , parseChar
        , parseString
        , parseBytes
        , parseFile
        ]


parse : String -> Result String ParsedLog
parse stringToParse =
    stringToParse
        |> String.trim
        |> P.run
            (P.succeed ParsedLog
                |= (P.getChompedString <| P.chompUntil ": ")
                |. P.token ": "
                |= parseValue
                |. P.end
            )
        |> Result.mapError deadEndsToString


parseWithOptionalTag : String -> Result String ParsedLog
parseWithOptionalTag stringToParse =
    stringToParse
        |> String.trim
        |> P.run
            (P.oneOf
                [ P.backtrackable
                    (P.succeed ParsedLog
                        |= (P.getChompedString <| P.chompUntil ": ")
                        |. P.token ": "
                        |= parseValue
                        |. P.end
                    )
                , P.succeed (ParsedLog "Debug message")
                    |= parseValue
                    |. P.end
                ]
            )
        |> Result.mapError deadEndsToString

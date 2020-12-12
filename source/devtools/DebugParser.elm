module DebugParser exposing (..)

import Expandable exposing (ElmValue(..), SequenceType(..))
import Parser as P exposing ((|.), (|=), DeadEnd, Parser, Step(..))


type alias ParsedLog =
    { tag : String
    , value : ElmValue
    }


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


parseBool : Parser ElmValue
parseBool =
    P.succeed ElmBool
        |= P.oneOf
            [ P.map (\_ -> True) (P.keyword "True")
            , P.map (\_ -> False) (P.keyword "False")
            ]


parseNumber : Parser ElmValue
parseNumber =
    P.oneOf
        [ P.succeed (ElmFloat (0 / 0))
            |. P.keyword "NaN"
        , P.succeed (ElmFloat (1 / 0))
            |. P.keyword "Infinity"
        , P.succeed (ElmFloat -(1 / 0))
            |. P.keyword "-Infinity"
        , P.oneOf
            [ P.succeed negate
                |. P.symbol "-"
                |= P.float
            , P.float
            ]
            |> P.map
                (\num ->
                    if num == (round num |> toFloat) then
                        ElmInt (round num)

                    else
                        ElmFloat num
                )
        ]


parseKeywords : Parser ElmValue
parseKeywords =
    P.oneOf
        [ P.succeed ElmInternals
            |. P.keyword "<internals>"
        , P.succeed ElmFunction
            |. P.keyword "<function>"
        ]


parseTuple : Parser ElmValue
parseTuple =
    P.oneOf
        [ P.succeed ElmUnit
            |. P.keyword "()"
        , P.backtrackable <|
            P.succeed (\fst snd -> ElmTuple False [ fst, snd ])
                |. P.token "("
                |= P.lazy (\_ -> parseValue)
                |. P.token ","
                |= P.lazy (\_ -> parseValue)
                |. P.token ")"
        , P.succeed (\fst snd rd -> ElmTuple False [ fst, snd, rd ])
            |. P.token "("
            |= P.lazy (\_ -> parseValue)
            |. P.token ","
            |= P.lazy (\_ -> parseValue)
            |. P.token ","
            |= P.lazy (\_ -> parseValue)
            |. P.token ")"
        ]


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
                ElmSequence List False listVal
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
                ElmSequence Array False listVal
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
                ElmSequence Set False listVal
            )



{- ---- String parser ------ -}


parseString : Parser ElmValue
parseString =
    P.succeed identity
        |. P.token "\""
        |= P.loop [] stringHelp
        |> P.andThen
            (Maybe.map (\str -> P.succeed (ElmString str))
                >> Maybe.withDefault (P.problem "One string has no closing double quotes")
            )


stringHelp : List String -> Parser (Step (List String) (Maybe String))
stringHelp revChunks =
    P.oneOf
        [ P.succeed (\chunk -> Loop (chunk :: revChunks))
            |. P.token "\\"
            |= P.oneOf
                [ P.map (\_ -> "\n") (P.token "n")
                , P.map (\_ -> "\t") (P.token "t")
                , P.map (\_ -> "\u{000D}") (P.token "r")
                , P.map (\_ -> "\\") (P.token "\\")
                , P.map (\_ -> "\"") (P.token "\"")
                , P.succeed String.fromChar
                    |. P.token "u{"
                    |= unicode
                    |. P.token "}"
                ]
        , P.oneOf
            [ P.token "\""
                |> P.map (\_ -> Done (Just <| String.join "" (List.reverse revChunks)))
            , P.end
                |> P.map (\_ -> Done Nothing)
            ]
        , P.chompWhile isUninteresting
            |> P.getChompedString
            |> P.map
                (\chunk ->
                    Loop (chunk :: revChunks)
                )
        ]


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
            |> P.map (\_ -> ElmChar '\'')
        , P.succeed identity
            |. P.token "'"
            |= P.getChompedString (P.chompUntil "'")
            |. P.token "'"
            |> P.map
                (String.toList >> List.head >> Maybe.withDefault 'x' >> ElmChar)
        ]



{--Record parser --}


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
                ElmRecord False listVal
            )



{--Custom type parser --}


parseTypeWithoutValue : Parser ElmValue
parseTypeWithoutValue =
    parseTypeName
        |> P.map
            (\typeName ->
                ElmType False typeName []
            )


parseCustomType : Parser ElmValue
parseCustomType =
    P.oneOf
        [ P.succeed (\name list -> ElmType False name (List.reverse list))
            |= parseTypeName
            |= P.loop [] typeHelp
        ]


typeHelp : List ElmValue -> Parser (Step (List ElmValue) (List ElmValue))
typeHelp values =
    P.oneOf
        [ P.backtrackable <|
            P.succeed (\value -> Loop (value :: values))
                |. P.token " "
                |= parseValue
        , P.succeed (\value -> Loop (value :: values))
            |. P.token " ("
            |= parseValue
            |. P.token ")"
        , P.succeed (Done values)
        ]



{--Dict parser-}


parseDict : Parser ElmValue
parseDict =
    let
        parseComparable =
            P.oneOf
                [ parseBool
                , parseNumber
                , parseTuple
                , parseChar
                , parseString
                ]
    in
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
                        |= parseComparable
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
                ElmDict False listVal
            )



{- Main value parser -}


parseUnit : Parser ElmValue
parseUnit =
    P.succeed ElmUnit
        |. P.keyword "()"


parseCustomTypeValue : Parser ElmValue
parseCustomTypeValue =
    P.oneOf
        [ parseUnit
        , parseRecord
        , parseArray
        , parseSet
        , parseDict
        , parseList
        , P.backtrackable parseNumber
        , P.backtrackable parseBool
        , parseKeywords
        , parseTuple
        , parseTypeWithoutValue
        , parseChar
        , parseString
        ]


parseValue : Parser ElmValue
parseValue =
    P.oneOf
        [ parseUnit
        , parseRecord
        , parseArray
        , parseSet
        , parseDict
        , parseList
        , P.backtrackable parseNumber
        , P.backtrackable parseBool
        , parseKeywords
        , P.lazy (\_ -> parseCustomType)
        , parseTuple
        , parseTypeWithoutValue
        , parseChar
        , parseString
        ]


parse : String -> Result (List DeadEnd) ParsedLog
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

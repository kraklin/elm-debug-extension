module DebugParser exposing (..)

import Expandable exposing (ElmValue(..), SequenceType(..))
import Parser exposing ((|.), (|=), DeadEnd)


type alias ParsedLog =
    { tag : String
    , value : ElmValue
    }


parseBool : Parser.Parser ElmValue
parseBool =
    Parser.succeed ElmBool
        |= Parser.oneOf
            [ Parser.map (\_ -> True) (Parser.keyword "True")
            , Parser.map (\_ -> False) (Parser.keyword "False")
            ]


parseNumber : Parser.Parser ElmValue
parseNumber =
    Parser.oneOf
        [ Parser.succeed (ElmFloat (0 / 0))
            |. Parser.keyword "NaN"
        , Parser.succeed (ElmFloat (1 / 0))
            |. Parser.keyword "Infinity"
        , Parser.succeed (ElmFloat -(1 / 0))
            |. Parser.keyword "-Infinity"
        , Parser.oneOf
            [ Parser.succeed negate
                |. Parser.symbol "-"
                |= Parser.float
            , Parser.float
            ]
            |> Parser.map
                (\num ->
                    if num == (round num |> toFloat) then
                        ElmInt (round num)

                    else
                        ElmFloat num
                )
        ]


parseKeywords : Parser.Parser ElmValue
parseKeywords =
    Parser.oneOf
        [ Parser.succeed ElmUnit
            |. Parser.keyword "()"
        , Parser.succeed ElmInternals
            |. Parser.keyword "<internals>"
        , Parser.succeed ElmFunction
            |. Parser.keyword "<function>"
        ]


parseTuple : Parser.Parser ElmValue
parseTuple =
    Parser.sequence
        { start = "("
        , end = ")"
        , separator = ","
        , spaces = Parser.spaces
        , item = Parser.lazy (\_ -> parseValue)
        , trailing = Parser.Forbidden
        }
        |> Parser.map
            (\listVal ->
                ElmTuple False listVal
            )


parseList : Parser.Parser ElmValue
parseList =
    Parser.sequence
        { start = "["
        , end = "]"
        , separator = ","
        , spaces = Parser.spaces
        , item = Parser.lazy (\_ -> parseValue)
        , trailing = Parser.Forbidden
        }
        |> Parser.map
            (\listVal ->
                ElmSequence List False listVal
            )


parseArray : Parser.Parser ElmValue
parseArray =
    Parser.sequence
        { start = "Array.fromList ["
        , end = "]"
        , separator = ","
        , spaces = Parser.spaces
        , item = Parser.lazy (\_ -> parseValue)
        , trailing = Parser.Forbidden
        }
        |> Parser.map
            (\listVal ->
                ElmSequence Array False listVal
            )


parseSet : Parser.Parser ElmValue
parseSet =
    Parser.sequence
        { start = "Set.fromList ["
        , end = "]"
        , separator = ","
        , spaces = Parser.spaces
        , item = Parser.lazy (\_ -> parseValue)
        , trailing = Parser.Forbidden
        }
        |> Parser.map
            (\listVal ->
                ElmSequence Set False listVal
            )


parseValue : Parser.Parser ElmValue
parseValue =
    Parser.oneOf
        [ parseKeywords
        , parseBool
        , parseNumber
        , parseTuple
        , parseArray
        , parseSet
        , parseList
        ]


parse : String -> Result (List DeadEnd) ParsedLog
parse stringToParse =
    Parser.run
        (Parser.succeed ParsedLog
            |= (Parser.getChompedString <| Parser.chompUntil ": ")
            |. Parser.token ": "
            |= parseValue
            |. Parser.end
        )
        stringToParse

module Decode exposing (..)

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Extra as Decode


type SequenceType
    = Tuple
    | Set
    | List
    | Array


type ElmValue
    = ElmString String
    | ElmInt Int
    | ElmFloat Float
    | ElmBool Bool
    | ElmSequence SequenceType (List ElmValue)
    | ElmRecord (List ( String, ElmValue ))
    | ElmFunction
    | ElmInternals
    | ElmUnit
    | ElmType String (Maybe (List ElmValue))
    | ElmDict (List ( ElmValue, ElmValue ))
    | ElmFile String
    | ElmBytes Int


type alias DecodedLog =
    { tag : String, value : ElmValue, hash : Int }


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
                        Decode.succeed (ElmSequence Tuple)
                            |> andDecodeValueField (Decode.list valueDecoder)

                    "Set" ->
                        Decode.succeed (ElmSequence Set)
                            |> andDecodeValueField (Decode.list valueDecoder)

                    "List" ->
                        Decode.succeed (ElmSequence List)
                            |> andDecodeValueField (Decode.list valueDecoder)

                    "Array" ->
                        Decode.succeed (ElmSequence Array)
                            |> andDecodeValueField (Decode.list valueDecoder)

                    "Record" ->
                        Decode.succeed ElmRecord
                            |> andDecodeValueField (Decode.keyValuePairs valueDecoder)

                    "Internals" ->
                        Decode.succeed ElmInternals

                    "Function" ->
                        Decode.succeed ElmFunction

                    "Unit" ->
                        Decode.succeed ElmUnit

                    "Type" ->
                        Decode.succeed ElmType
                            |> Decode.andMap (Decode.field "name" Decode.string)
                            |> Decode.andMap (Decode.succeed Nothing)

                    "Custom" ->
                        Decode.succeed ElmType
                            |> Decode.andMap (Decode.field "name" Decode.string)
                            |> Decode.andMap (Decode.maybe (Decode.field "value" (Decode.list valueDecoder)))

                    "Dict" ->
                        Decode.succeed ElmDict
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

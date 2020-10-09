module Decode exposing (..)

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Extra as Decode


type ElmValue
    = ElmString String
    | ElmInt Int
    | ElmBool Bool

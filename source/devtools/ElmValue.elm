module ElmValue exposing (ElmValue(..), SequenceType(..))


type SequenceType
    = SeqSet
    | SeqList
    | SeqArray
    | SeqTuple


type ElmValue
    = ElmString String
    | ElmChar Char
    | ElmNumber Float
    | ElmBool Bool
    | ElmFunction
    | ElmInternals
    | ElmUnit
    | ElmFile String
    | ElmBytes Int
    | ElmSequence Bool SequenceType (List ElmValue)
    | ElmType Bool String (List ElmValue)
    | ElmRecord Bool (List ( String, ElmValue ))
    | ElmDict Bool (List ( ElmValue, ElmValue ))

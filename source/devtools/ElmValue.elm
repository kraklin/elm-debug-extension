module ElmValue exposing (ElmValue(..), SequenceType(..))


type SequenceType
    = SeqSet
    | SeqList
    | SeqArray


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
    | ElmSequence SequenceType Bool (List ElmValue)
    | ElmTuple Bool (List ElmValue)
    | ElmRecord Bool (List ( String, ElmValue ))
    | ElmType Bool String (List ElmValue)
    | ElmDict Bool (List ( ElmValue, ElmValue ))

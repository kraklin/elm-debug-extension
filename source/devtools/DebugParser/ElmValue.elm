module DebugParser.ElmValue exposing (ElmValue(..), SequenceType(..), hasNestedValues, toggle)


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


hasNestedValues : ElmValue -> Bool
hasNestedValues value =
    case value of
        ElmSequence _ _ values ->
            not <| List.isEmpty values

        ElmRecord _ _ ->
            True

        ElmDict _ values ->
            not <| List.isEmpty values

        ElmType _ _ values ->
            not <| List.isEmpty values

        _ ->
            False


toggle : ElmValue -> ElmValue
toggle value =
    case value of
        ElmSequence isOpened seq values ->
            ElmSequence (not isOpened) seq values

        ElmRecord isOpened values ->
            ElmRecord (not isOpened) values

        ElmDict isOpened values ->
            ElmDict (not isOpened) values

        ElmType isOpened name values ->
            case values of
                [] ->
                    value

                _ ->
                    ElmType (not isOpened) name values

        _ ->
            value

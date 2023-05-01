module DebugParser.ElmValue exposing (ElmValue(..), ExpandableValue(..), PlainValue(..), SequenceType(..), hasNestedValues, toggle)

import Html.Attributes exposing (value)


type SequenceType
    = SeqSet
    | SeqList
    | SeqArray
    | SeqTuple


type ElmValue
    = Plain PlainValue
    | Expandable Bool ExpandableValue


type PlainValue
    = ElmString String
    | ElmChar Char
    | ElmNumber Float
    | ElmBool Bool
    | ElmFunction
    | ElmInternals
    | ElmUnit
    | ElmFile String
    | ElmBytes Int


type ExpandableValue
    = ElmSequence SequenceType (List ElmValue)
    | ElmType String (List ElmValue)
    | ElmRecord (List ( String, ElmValue ))
    | ElmDict (List ( ElmValue, ElmValue ))


hasNestedValues : ElmValue -> Bool
hasNestedValues value =
    case value of
        Expandable _ expandableValue ->
            case expandableValue of
                ElmSequence _ values ->
                    not <| List.isEmpty values

                ElmRecord _ ->
                    True

                ElmDict values ->
                    not <| List.isEmpty values

                ElmType _ values ->
                    not <| List.isEmpty values

        _ ->
            False


toggle : ElmValue -> ElmValue
toggle value =
    case value of
        Expandable isOpened expandableValue ->
            case expandableValue of
                ElmType _ [] ->
                    value

                _ ->
                    Expandable (not isOpened) expandableValue

        _ ->
            value

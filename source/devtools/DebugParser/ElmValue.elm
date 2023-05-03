module DebugParser.ElmValue exposing
    ( ElmValue(..)
    , ExpandableValue(..)
    , Path
    , PlainValue(..)
    , SequenceType(..)
    , hasNestedValues
    , mapValuesWithPath
    , toggle
    , toggleValueByPath
    )

import List.Extra as List


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


type SequenceType
    = SeqSet
    | SeqList
    | SeqArray
    | SeqTuple


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


type alias Path =
    List Int


mapValuesWithPath : (Path -> ElmValue -> a) -> Path -> List ElmValue -> List a
mapValuesWithPath mapFn path values =
    let
        newPath idx =
            path ++ [ idx ]
    in
    List.indexedMap (\idx value -> mapFn (newPath idx) value) values


toggleValueByPath : Path -> ElmValue -> ElmValue
toggleValueByPath path value =
    let
        mapNestedValue mapIndex mappedFn =
            case value of
                Expandable isOpened expandableValue ->
                    Expandable isOpened <|
                        case expandableValue of
                            ElmSequence b values ->
                                ElmSequence b <| List.updateIfIndex ((==) mapIndex) mappedFn values

                            ElmRecord values ->
                                ElmRecord <| List.updateIfIndex ((==) mapIndex) (Tuple.mapSecond mappedFn) values

                            ElmDict values ->
                                ElmDict <| List.updateIfIndex ((==) mapIndex) (Tuple.mapSecond mappedFn) values

                            ElmType b typeValues ->
                                case typeValues of
                                    [] ->
                                        ElmType b typeValues

                                    values ->
                                        ElmType b <| List.updateIfIndex ((==) mapIndex) mappedFn values

                _ ->
                    value
    in
    case path of
        [] ->
            toggle value

        [ idx ] ->
            mapNestedValue idx toggle

        idx :: rest ->
            mapNestedValue idx (toggleValueByPath rest)

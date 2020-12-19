module DebugMessages exposing (..)

import DebugParser
import Dict exposing (Dict)
import Expandable exposing (ElmValue)
import List.Extra as List
import Murmur3


type alias StoreMessage =
    { tag : String
    , value : ElmValue
    }


type alias DebugMessage =
    { tag : String
    , value : ElmValue
    , key : Key
    , count : Int
    }


type alias AddMessageData =
    { timestamp : String
    , log : String
    }


type alias Key =
    ( Int, String )


type DebugMessages
    = DebugMessages
        { store : Dict Key StoreMessage
        , queue : List ( Key, Int )
        }


empty : DebugMessages
empty =
    DebugMessages
        { store = Dict.empty
        , queue = []
        }


messageHash : String -> Int
messageHash input =
    Murmur3.hashString 8532 input


add : AddMessageData -> DebugMessages -> DebugMessages
add { timestamp, log } (DebugMessages data) =
    let
        hash =
            messageHash log

        lastHash =
            data.queue
                |> List.head
                |> Maybe.map (Tuple.first >> Tuple.first)

        dictKey =
            ( hash, timestamp )

        increaseLastCount queue =
            List.updateAt 0 (Tuple.mapSecond (\s -> s + 1)) queue
    in
    if lastHash == Just hash then
        DebugMessages { data | queue = increaseLastCount data.queue }

    else
        case DebugParser.parse log of
            Ok { tag, value } ->
                DebugMessages
                    { store =
                        Dict.insert dictKey
                            { tag = tag
                            , value = value
                            }
                            data.store
                    , queue = ( dictKey, 1 ) :: data.queue
                    }

            Err e ->
                DebugMessages data


bulkAdd : List AddMessageData -> DebugMessages -> DebugMessages
bulkAdd bulkList (DebugMessages data) =
    Debug.todo "add bulk messages"



-- DebugMessages data
{-

   |> List.groupWhile (\v1 v2 -> v1.hash == v2.hash)
   |> List.map (\( v, list ) -> ( v, List.length list + 1 ))
   |> List.map
       (\( { hash, log, time }, count ) ->
           case DebugParser.parse log of
               Ok { tag, value } ->
                   Just
                       { count = count
                       , tag = tag
                       , value = value
                       , hash = hash
                       , isoTimestamp = time
                       }

               Err _ ->
                   Nothing
       )
   |> List.filterMap identity

-}


messages : DebugMessages -> List DebugMessage
messages (DebugMessages data) =
    data.queue
        |> List.map
            (\( key, count ) ->
                Dict.get key data.store
                    |> Maybe.map
                        (\m ->
                            { tag = m.tag
                            , value = m.value
                            , count = count
                            , key = key
                            }
                        )
            )
        |> List.filterMap identity


toggleValue : Key -> Expandable.Key -> DebugMessages -> DebugMessages
toggleValue key path (DebugMessages data) =
    DebugMessages
        { data
            | store =
                Dict.update key
                    (Maybe.map
                        (\storeMessage ->
                            { storeMessage
                                | value = Expandable.map path Expandable.toggle storeMessage.value
                            }
                        )
                    )
                    data.store
        }

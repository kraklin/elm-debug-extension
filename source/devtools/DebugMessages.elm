module DebugMessages exposing
    ( AddMessageData
    , DebugMessage
    , DebugMessages
    , Key
    , add
    , bulkAdd
    , clear
    , holdOnQueueSize
    , init
    , initWithCustomParser
    , isHoldOn
    , mergeHoldOnQueue
    , messages
    , setHoldOff
    , setHoldOn
    , toggleValue
    )

import DebugParser exposing (ParsedLog)
import Dict exposing (Dict)
import Expandable exposing (ElmValue)
import List.Extra as List
import Murmur3
import Time exposing (Posix)


type alias StoreMessage =
    { tag : String
    , value : ElmValue
    }


type alias DebugMessage =
    { tag : String
    , value : ElmValue
    , key : Key
    , count : Int
    , time : Posix
    }


type alias AddMessageData =
    { timestamp : Posix
    , log : String
    }


type alias Hash =
    Int


{-|

    first: hash
    second: Posix.fromMilis

-}
type alias Key =
    ( Hash, Int )


toKey : Hash -> Posix -> Key
toKey hash time =
    ( hash, Time.posixToMillis time )


posixFromKey : Key -> Posix
posixFromKey =
    Tuple.second
        >> Time.millisToPosix


hashFromKey : Key -> Hash
hashFromKey =
    Tuple.first


type DebugMessages
    = DebugMessages
        { store : Dict Key StoreMessage
        , queue : List ( Key, Int )
        , holdOnQueue : List ( Key, Int )
        , holdOn : Bool
        , parseFn : String -> Result String ParsedLog
        }


initWithCustomParser : (String -> Result String ParsedLog) -> DebugMessages
initWithCustomParser parseFn =
    DebugMessages
        { store = Dict.empty
        , queue = []
        , holdOnQueue = []
        , holdOn = False
        , parseFn = parseFn
        }


init : DebugMessages
init =
    initWithCustomParser DebugParser.parse


clear : DebugMessages -> DebugMessages
clear (DebugMessages { parseFn, holdOn }) =
    DebugMessages
        { store = Dict.empty
        , queue = []
        , holdOnQueue = []
        , holdOn = holdOn
        , parseFn = parseFn
        }


isHoldOn : DebugMessages -> Bool
isHoldOn (DebugMessages { holdOn }) =
    holdOn


setHoldOn : DebugMessages -> DebugMessages
setHoldOn (DebugMessages data) =
    DebugMessages { data | holdOn = True }


setHoldOff : DebugMessages -> DebugMessages
setHoldOff (DebugMessages data) =
    DebugMessages
        { data
            | holdOn = False
            , queue = data.queue ++ data.holdOnQueue
            , holdOnQueue = []
        }


messageHash : String -> Int
messageHash input =
    Murmur3.hashString 8537 input


add : AddMessageData -> DebugMessages -> Result String DebugMessages
add { timestamp, log } (DebugMessages data) =
    let
        hash =
            messageHash log

        lastHash =
            data.queue
                |> List.head
                |> Maybe.map (Tuple.first >> hashFromKey)

        lastHoldOnHash =
            data.holdOnQueue
                |> List.head
                |> Maybe.map (Tuple.first >> hashFromKey)

        dictKey =
            toKey hash timestamp

        increaseLastCount queue =
            List.updateAt 0 (Tuple.mapSecond (\s -> s + 1)) queue

        updateQueues data_ =
            if data_.holdOn then
                { data_
                    | holdOnQueue = ( dictKey, 1 ) :: data_.holdOnQueue
                }

            else if lastHash == Just hash then
                { data_ | queue = increaseLastCount data_.queue }

            else
                { data_ | queue = ( dictKey, 1 ) :: data_.queue }
    in
    if data.holdOn then
        if lastHoldOnHash == Just hash then
            Ok <| DebugMessages { data | holdOnQueue = increaseLastCount data.holdOnQueue }

        else
            data.parseFn log
                |> Result.map
                    (\{ tag, value } ->
                        DebugMessages
                            { data
                                | store =
                                    Dict.insert dictKey
                                        { tag = tag
                                        , value = value
                                        }
                                        data.store
                                , holdOnQueue = ( dictKey, 1 ) :: data.holdOnQueue
                            }
                    )

    else if lastHash == Just hash then
        Ok <| DebugMessages { data | queue = increaseLastCount data.queue }

    else
        data.parseFn log
            |> Result.map
                (\{ tag, value } ->
                    DebugMessages
                        { data
                            | store =
                                Dict.insert dictKey
                                    { tag = tag
                                    , value = value
                                    }
                                    data.store
                            , queue = ( dictKey, 1 ) :: data.queue
                        }
                )


bulkAdd : List AddMessageData -> DebugMessages -> DebugMessages
bulkAdd bulkList (DebugMessages data) =
    let
        parsedMessages =
            bulkList
                |> List.map (\messageData -> ( messageHash messageData.log, messageData ))
                |> List.groupWhile (\v1 v2 -> Tuple.first v1 == Tuple.first v2)
                |> List.map (\( v, list ) -> ( v, List.length list + 1 ))
                |> List.map
                    (\( ( hash, { log, timestamp } ), count ) ->
                        case data.parseFn log of
                            Ok { tag, value } ->
                                Just
                                    { count = count
                                    , tag = tag
                                    , value = value
                                    , hash = hash
                                    , timestamp = timestamp
                                    }

                            Err _ ->
                                Nothing
                    )
                |> List.filterMap identity

        newStore =
            parsedMessages
                |> List.map (\{ tag, value, hash, timestamp } -> ( toKey hash timestamp, { tag = tag, value = value } ))
                |> Dict.fromList
                |> Dict.union data.store

        newQueue =
            parsedMessages
                |> List.map (\{ hash, timestamp, count } -> ( toKey hash timestamp, count ))
    in
    if data.holdOn then
        DebugMessages { data | store = newStore, holdOnQueue = newQueue ++ data.holdOnQueue }

    else
        DebugMessages { data | store = newStore, queue = newQueue ++ data.queue }


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
                            , time = posixFromKey key
                            }
                        )
            )
        |> List.filterMap identity


holdOnQueueSize : DebugMessages -> Int
holdOnQueueSize (DebugMessages { holdOnQueue }) =
    List.length holdOnQueue


mergeHoldOnQueue : DebugMessages -> DebugMessages
mergeHoldOnQueue (DebugMessages data) =
    DebugMessages { data | queue = data.holdOnQueue ++ data.queue, holdOnQueue = [] }


toggleValue : Key -> Expandable.Key -> DebugMessages -> DebugMessages
toggleValue key path (DebugMessages data) =
    let
        toggleValue_ storeMessage =
            { storeMessage
                | value = Expandable.map path Expandable.toggle storeMessage.value
            }
    in
    DebugMessages
        { data
            | store =
                Dict.update key (Maybe.map toggleValue_) data.store
        }

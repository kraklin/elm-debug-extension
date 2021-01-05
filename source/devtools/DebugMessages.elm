module DebugMessages exposing (..)

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
        , parseFn : String -> Result String ParsedLog
        }


initWithCustomParser : (String -> Result String ParsedLog) -> DebugMessages
initWithCustomParser parseFn =
    DebugMessages
        { store = Dict.empty
        , queue = []
        , parseFn = parseFn
        }


init : DebugMessages
init =
    DebugMessages
        { store = Dict.empty
        , queue = []
        , parseFn = DebugParser.parse
        }


clear : DebugMessages -> DebugMessages
clear (DebugMessages { parseFn }) =
    DebugMessages
        { store = Dict.empty
        , queue = []
        , parseFn = parseFn
        }


messageHash : String -> Int
messageHash input =
    Murmur3.hashString 8532 input


add : AddMessageData -> DebugMessages -> Result String DebugMessages
add { timestamp, log } (DebugMessages data) =
    let
        hash =
            messageHash log

        lastHash =
            data.queue
                |> List.head
                |> Maybe.map (Tuple.first >> hashFromKey)

        dictKey =
            toKey hash timestamp

        increaseLastCount queue =
            List.updateAt 0 (Tuple.mapSecond (\s -> s + 1)) queue
    in
    if lastHash == Just hash then
        Ok <| DebugMessages { data | queue = increaseLastCount data.queue }

    else
        data.parseFn log
            |> Result.map
                (\{ tag, value } ->
                    DebugMessages
                        { store =
                            Dict.insert dictKey
                                { tag = tag
                                , value = value
                                }
                                data.store
                        , queue = ( dictKey, 1 ) :: data.queue
                        , parseFn = data.parseFn
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

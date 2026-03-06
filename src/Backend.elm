module Backend exposing (..)

import Dict exposing (Dict)
import Lamdera exposing (ClientId, SessionId, sendToFrontend)
import Task
import Time
import Types exposing (..)


type alias Model =
    BackendModel


type alias PingProcessState =
    { rooms : Dict RoomId Room
    , timedOutUsers : List ( ClientId, String )
    , cmds : List (Cmd BackendMsg)
    , closedRoomIds : List RoomId
    }


hiddenTabGraceTicks : Int
hiddenTabGraceTicks =
    360


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { rooms = Dict.empty
      , clientToRoom = Dict.empty
      , statsViewers = Dict.empty
      , activeSessionStats = Dict.empty
      , completedSessionsByDay = Dict.empty
      , currentDayKey = "1970-01-01"
      , currentDayIndex = 0
      }
    , Time.now |> Task.perform InitializeCurrentDay
    )


subscriptions : Model -> Sub BackendMsg
subscriptions _ =
    Sub.batch
        [ Lamdera.onConnect ClientConnected
        , Lamdera.onDisconnect ClientDisconnected
        , Time.every 5000 PingTick
        ]


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        ClientConnected _ clientId ->
            -- Client connected, nothing to do until they join a room
            ( model, Cmd.none )

        InitializeCurrentDay now ->
            ( updateCurrentDay now model, Cmd.none )

        ClientDisconnected _ clientId ->
            -- Remove from stats viewers
            let
                modelWithoutStatsViewer =
                    { model | statsViewers = Dict.remove clientId model.statsViewers }
            in
            -- Mark participant as disconnected and check if room should be cleaned up
            case Dict.get clientId modelWithoutStatsViewer.clientToRoom of
                Nothing ->
                    -- Client wasn't in a room, nothing else to do
                    ( modelWithoutStatsViewer, Cmd.none )

                Just roomId ->
                    let
                        newModel =
                            { modelWithoutStatsViewer | clientToRoom = Dict.remove clientId modelWithoutStatsViewer.clientToRoom }

                        updatedRooms =
                            Dict.update roomId
                                (Maybe.map (markParticipantDisconnected clientId))
                                newModel.rooms

                        finalModel =
                            { newModel | rooms = updatedRooms }

                        -- Check if room should be removed (no connected participants)
                        ( cleanedModel, roomClosed ) =
                            cleanupRoomIfEmpty roomId finalModel

                        -- Broadcast stats update to stats viewers
                        statsCmds =
                            broadcastStatsUpdate cleanedModel
                    in
                    if roomClosed then
                        -- Notify remaining clients that room is closed
                        ( cleanedModel, statsCmds )

                    else
                        -- Broadcast updated room state to remaining participants
                        case Dict.get roomId cleanedModel.rooms of
                            Just room ->
                                ( cleanedModel, Cmd.batch [ broadcastRoomUpdate room cleanedModel, statsCmds ] )

                            Nothing ->
                                ( cleanedModel, statsCmds )

        PingTick now ->
            -- Increment missed pongs for all participants and check for timeouts
            let
                modelWithCurrentDay =
                    updateCurrentDay now model

                pingState =
                    Dict.foldl
                        (processRoomPing modelWithCurrentDay)
                        { rooms = Dict.empty
                        , timedOutUsers = []
                        , cmds = []
                        , closedRoomIds = []
                        }
                        modelWithCurrentDay.rooms

                -- Remove timed out users from clientToRoom
                updatedClientToRoom =
                    List.foldl
                        (\( cId, _ ) acc -> Dict.remove cId acc)
                        modelWithCurrentDay.clientToRoom
                        pingState.timedOutUsers

                baseModel =
                    { modelWithCurrentDay
                        | rooms = pingState.rooms
                        , clientToRoom = updatedClientToRoom
                    }

                newModel =
                    archiveClosedSessions pingState.closedRoomIds baseModel

                -- Send pings to all connected clients
                pingCmds =
                    Dict.keys newModel.clientToRoom
                        |> List.map (\cId -> sendToFrontend cId Ping)

                -- Broadcast stats update if anyone timed out
                statsCmds =
                    if List.isEmpty pingState.timedOutUsers then
                        []

                    else
                        [ broadcastStatsUpdate newModel ]
            in
            ( newModel, Cmd.batch (pingState.cmds ++ pingCmds ++ statsCmds) )

        NoOpBackendMsg ->
            ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        CreateRoom roomName userName ->
            let
                roomId =
                    generateRoomId clientId

                pin =
                    generatePin clientId

                participant =
                    { name = userName
                    , vote = Nothing
                    , isConnected = True
                    , missedPongs = 0
                    , tabHidden = False
                    , hiddenPingTicks = 0
                    }

                room =
                    { id = roomId
                    , pin = pin
                    , name = roomName
                    , participants = Dict.singleton clientId participant
                    , votesRevealed = False
                    }

                newModel =
                    startActiveSession roomId 1
                        { model
                        | rooms = Dict.insert roomId room model.rooms
                        , clientToRoom = Dict.insert clientId roomId model.clientToRoom
                    }
            in
            ( newModel
            , Cmd.batch
                [ sendToFrontend clientId (RoomCreated room clientId)
                , broadcastStatsUpdate newModel
                ]
            )

        JoinRoom roomId pin userName ->
            case Dict.get roomId model.rooms of
                Nothing ->
                    ( model, sendToFrontend clientId RoomNotFound )

                Just room ->
                    if room.pin /= pin then
                        ( model, sendToFrontend clientId InvalidPin )

                    else
                        let
                            participant =
                                { name = userName
                                , vote = Nothing
                                , isConnected = True
                                , missedPongs = 0
                                , tabHidden = False
                                , hiddenPingTicks = 0
                                }

                            updatedRoom =
                                { room | participants = Dict.insert clientId participant room.participants }

                            newModel =
                                incrementActiveSessionParticipants roomId
                                    { model
                                    | rooms = Dict.insert roomId updatedRoom model.rooms
                                    , clientToRoom = Dict.insert clientId roomId model.clientToRoom
                                }
                        in
                        ( newModel
                        , Cmd.batch
                            [ sendToFrontend clientId (RoomJoined updatedRoom clientId)
                            , broadcastRoomUpdate updatedRoom newModel
                            , broadcastStatsUpdate newModel
                            ]
                        )

        SubmitVote vote ->
            updateRoomAndSessionForClient clientId model <|
                \room session ->
                    let
                        updatedRoom =
                            { room
                                | participants =
                                    Dict.update clientId
                                        (Maybe.map (\p -> { p | vote = Just vote }))
                                        room.participants
                            }

                        finalRoom =
                            checkAutoReveal updatedRoom

                        updatedSession =
                            { session | currentRoundVotes = Dict.insert clientId vote session.currentRoundVotes }
                    in
                    ( finalRoom, updatedSession )

        ClearMyVote ->
            updateRoomAndSessionForClient clientId model <|
                \room session ->
                    ( { room
                        | participants =
                            Dict.update clientId
                                (Maybe.map (\p -> { p | vote = Nothing }))
                                room.participants
                      }
                    , { session | currentRoundVotes = Dict.remove clientId session.currentRoundVotes }
                    )

        RevealVotes ->
            updateRoomAndSessionForClient clientId model <|
                \room session ->
                    ( { room | votesRevealed = True }, session )

        ResetVotes ->
            updateRoomAndSessionForClient clientId model <|
                \room session ->
                    ( { room
                        | votesRevealed = False
                        , participants =
                            Dict.map (\_ p -> { p | vote = Nothing }) room.participants
                      }
                    , { session | currentRoundVotes = Dict.empty }
                    )

        LeaveRoom ->
            case Dict.get clientId model.clientToRoom of
                Nothing ->
                    ( model, Cmd.none )

                Just roomId ->
                    let
                        newModel =
                            { model
                                | clientToRoom = Dict.remove clientId model.clientToRoom
                                , rooms =
                                    Dict.update roomId
                                        (Maybe.map (\room -> { room | participants = Dict.remove clientId room.participants }))
                                        model.rooms
                            }

                        ( cleanedModel, _ ) =
                            cleanupRoomIfEmpty roomId newModel

                        statsCmds =
                            broadcastStatsUpdate cleanedModel
                    in
                    case Dict.get roomId cleanedModel.rooms of
                        Just room ->
                            ( cleanedModel, Cmd.batch [ broadcastRoomUpdate room cleanedModel, statsCmds ] )

                        Nothing ->
                            ( cleanedModel, statsCmds )

        SubscribeToStats filters ->
            let
                newModel =
                    { model | statsViewers = Dict.insert clientId filters model.statsViewers }
            in
            ( newModel, sendToFrontend clientId (StatsData (buildStats filters newModel)) )

        UpdateStatsFilters filters ->
            let
                newModel =
                    { model | statsViewers = Dict.insert clientId filters model.statsViewers }
            in
            ( newModel, sendToFrontend clientId (StatsData (buildStats filters newModel)) )

        UnsubscribeFromStats ->
            ( { model | statsViewers = Dict.remove clientId model.statsViewers }, Cmd.none )

        Pong ->
            handleHeartbeatResponse False clientId model

        VisibilityPong ->
            handleHeartbeatResponse True clientId model

        TabHidden ->
            updateRoomForClient clientId model <|
                \room ->
                    { room
                        | participants =
                            Dict.update clientId
                                (Maybe.map
                                    (\p ->
                                        { p
                                            | missedPongs = 0
                                            , tabHidden = True
                                            , hiddenPingTicks = 0
                                        }
                                    )
                                )
                                room.participants
                    }



-- =============================================================================
-- Helper Functions
-- =============================================================================


{-| Update room for a client and broadcast the change
-}
updateRoomForClient : ClientId -> Model -> (Room -> Room) -> ( Model, Cmd BackendMsg )
updateRoomForClient clientId model updateFn =
    case Dict.get clientId model.clientToRoom of
        Nothing ->
            ( model, Cmd.none )

        Just roomId ->
            case Dict.get roomId model.rooms of
                Nothing ->
                    ( model, Cmd.none )

                Just room ->
                    let
                        updatedRoom =
                            updateFn room

                        newModel =
                            { model | rooms = Dict.insert roomId updatedRoom model.rooms }
                    in
                    ( newModel
                    , Cmd.batch
                        [ broadcastRoomUpdate updatedRoom newModel
                        , broadcastStatsUpdate newModel
                        ]
                    )


updateRoomAndSessionForClient : ClientId -> Model -> (Room -> SessionStats -> ( Room, SessionStats )) -> ( Model, Cmd BackendMsg )
updateRoomAndSessionForClient clientId model updateFn =
    case Dict.get clientId model.clientToRoom of
        Nothing ->
            ( model, Cmd.none )

        Just roomId ->
            case ( Dict.get roomId model.rooms, Dict.get roomId model.activeSessionStats ) of
                ( Just room, Just sessionStats ) ->
                    let
                        ( updatedRoomBeforeFinalize, updatedSessionBeforeFinalize ) =
                            updateFn room sessionStats

                        finalizedSession =
                            if not room.votesRevealed && updatedRoomBeforeFinalize.votesRevealed then
                                finalizeSessionRound updatedSessionBeforeFinalize

                            else
                                updatedSessionBeforeFinalize

                        newModel =
                            { model
                                | rooms = Dict.insert roomId updatedRoomBeforeFinalize model.rooms
                                , activeSessionStats = Dict.insert roomId finalizedSession model.activeSessionStats
                            }
                    in
                    ( newModel
                    , Cmd.batch
                        [ broadcastRoomUpdate updatedRoomBeforeFinalize newModel
                        , broadcastStatsUpdate newModel
                        ]
                    )

                _ ->
                    ( model, Cmd.none )


{-| Broadcast room update to all participants in the room
-}
broadcastRoomUpdate : Room -> Model -> Cmd BackendMsg
broadcastRoomUpdate room model =
    room.participants
        |> Dict.keys
        |> List.map (\cId -> sendToFrontend cId (RoomUpdated room))
        |> Cmd.batch


{-| Mark a participant as disconnected
-}
markParticipantDisconnected : ClientId -> Room -> Room
markParticipantDisconnected clientId room =
    { room
        | participants =
            Dict.update clientId
                (Maybe.map (\p -> { p | isConnected = False, tabHidden = False, hiddenPingTicks = 0 }))
                room.participants
    }


{-| Check if room should be removed (no connected participants)
Returns updated model and whether room was closed
-}
cleanupRoomIfEmpty : RoomId -> Model -> ( Model, Bool )
cleanupRoomIfEmpty roomId model =
    case Dict.get roomId model.rooms of
        Nothing ->
            ( model, False )

        Just room ->
            let
                connectedCount =
                    Dict.filter (\_ p -> p.isConnected) room.participants
                        |> Dict.size
            in
            if connectedCount == 0 then
                ( archiveClosedSession roomId { model | rooms = Dict.remove roomId model.rooms }, True )

            else
                ( model, False )


{-| Auto-reveal votes when all connected participants have voted
-}
checkAutoReveal : Room -> Room
checkAutoReveal room =
    let
        connectedParticipants =
            Dict.filter (\_ p -> p.isConnected) room.participants

        allVoted =
            connectedParticipants
                |> Dict.values
                |> List.all (\p -> p.vote /= Nothing)
    in
    if allVoted && Dict.size connectedParticipants > 0 && not room.votesRevealed then
        { room | votesRevealed = True }

    else
        room


{-| Generate a simple room ID based on client ID (deterministic for now)
In production, you'd want proper random generation
-}
generateRoomId : ClientId -> RoomId
generateRoomId clientId =
    clientId
        |> String.filter Char.isAlphaNum
        |> String.left 8
        |> String.toLower


{-| Generate a simple 4-digit PIN based on client ID (deterministic for now)
In production, you'd want proper random generation
-}
generatePin : ClientId -> Pin
generatePin clientId =
    clientId
        |> String.toList
        |> List.map Char.toCode
        |> List.foldl (+) 0
        |> modBy 10000
        |> String.fromInt
        |> String.padLeft 4 '0'


{-| Reset missed pongs counter for a participant
-}
resetMissedPongs : ClientId -> Room -> Room
resetMissedPongs clientId room =
    { room
        | participants =
            Dict.update clientId
                (Maybe.map (\p -> { p | missedPongs = 0 }))
                room.participants
    }


startActiveSession : RoomId -> Int -> Model -> Model
startActiveSession roomId participantCount model =
    { model
        | activeSessionStats =
            Dict.insert roomId
                { startedDayKey = model.currentDayKey
                , startedDayIndex = model.currentDayIndex
                , participantCount = participantCount
                , totalVotes = 0
                , reachedReveal = False
                , cardCounts = Dict.empty
                , currentRoundVotes = Dict.empty
                }
                model.activeSessionStats
    }


incrementActiveSessionParticipants : RoomId -> Model -> Model
incrementActiveSessionParticipants roomId model =
    { model
        | activeSessionStats =
            Dict.update roomId
                (Maybe.map (\sessionStats -> { sessionStats | participantCount = sessionStats.participantCount + 1 }))
                model.activeSessionStats
    }


finalizeSessionRound : SessionStats -> SessionStats
finalizeSessionRound sessionStats =
    let
        roundVoteCount =
            Dict.size sessionStats.currentRoundVotes

        updatedCardCounts =
            Dict.foldl
                (\_ vote acc -> Dict.update (voteKey vote) (incrementMaybe 1) acc)
                sessionStats.cardCounts
                sessionStats.currentRoundVotes
    in
    { sessionStats
        | totalVotes = sessionStats.totalVotes + roundVoteCount
        , reachedReveal = sessionStats.reachedReveal || roundVoteCount > 0
        , cardCounts = updatedCardCounts
        , currentRoundVotes = Dict.empty
    }


archiveClosedSession : RoomId -> Model -> Model
archiveClosedSession roomId model =
    case Dict.get roomId model.activeSessionStats of
        Nothing ->
            { model | activeSessionStats = Dict.remove roomId model.activeSessionStats }

        Just sessionStats ->
            let
                archivedSession =
                    if Dict.isEmpty sessionStats.currentRoundVotes then
                        { sessionStats | currentRoundVotes = Dict.empty }

                    else
                        finalizeSessionRound sessionStats
            in
            { model
                | activeSessionStats = Dict.remove roomId model.activeSessionStats
                , completedSessionsByDay =
                    Dict.update archivedSession.startedDayKey
                        (\maybeSessions ->
                            Just (archivedSession :: Maybe.withDefault [] maybeSessions)
                        )
                        model.completedSessionsByDay
            }


archiveClosedSessions : List RoomId -> Model -> Model
archiveClosedSessions roomIds model =
    List.foldl archiveClosedSession model roomIds


updateCurrentDay : Time.Posix -> Model -> Model
updateCurrentDay now model =
    { model
        | currentDayKey = dayKeyFromPosix now
        , currentDayIndex = dayIndexFromPosix now
    }


dayIndexFromPosix : Time.Posix -> Int
dayIndexFromPosix now =
    Time.posixToMillis now // 86400000


dayKeyFromPosix : Time.Posix -> String
dayKeyFromPosix now =
    String.fromInt (Time.toYear Time.utc now)
        ++ "-"
        ++ padDayComponent (monthToInt (Time.toMonth Time.utc now))
        ++ "-"
        ++ padDayComponent (Time.toDay Time.utc now)


padDayComponent : Int -> String
padDayComponent value =
    String.fromInt value |> String.padLeft 2 '0'


monthToInt : Time.Month -> Int
monthToInt month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


incrementMaybe : Int -> Maybe Int -> Maybe Int
incrementMaybe amount maybeValue =
    Just (Maybe.withDefault 0 maybeValue + amount)


voteKey : Vote -> String
voteKey vote =
    case vote of
        NumericVote n ->
            String.fromInt n

        HalfPoint ->
            "half"

        QuestionMark ->
            "question"

        CoffeeBreak ->
            "coffee"


handleHeartbeatResponse : Bool -> ClientId -> Model -> ( Model, Cmd BackendMsg )
handleHeartbeatResponse clearHiddenState clientId model =
    case Dict.get clientId model.clientToRoom of
        Nothing ->
            ( model, Cmd.none )

        Just roomId ->
            case Dict.get roomId model.rooms of
                Nothing ->
                    ( model, Cmd.none )

                Just room ->
                    let
                        participantBeforeUpdate =
                            Dict.get clientId room.participants

                        wasCritical =
                            participantBeforeUpdate
                                |> Maybe.map (\p -> p.missedPongs >= 2)
                                |> Maybe.withDefault False

                        wasHidden =
                            participantBeforeUpdate
                                |> Maybe.map .tabHidden
                                |> Maybe.withDefault False

                        updatedRoom =
                            resetMissedPongs clientId room
                                |> (\updated ->
                                        if clearHiddenState then
                                            { updated
                                                | participants =
                                                    Dict.update clientId
                                                        (Maybe.map (\p -> { p | tabHidden = False, hiddenPingTicks = 0 }))
                                                        updated.participants
                                            }

                                        else
                                            updated
                                   )

                        newModel =
                            { model | rooms = Dict.insert roomId updatedRoom model.rooms }
                    in
                    if wasHidden then
                        ( newModel
                        , Cmd.batch
                            [ broadcastRoomUpdate updatedRoom newModel
                            , broadcastStatsUpdate newModel
                            ]
                        )

                    else if wasCritical then
                        ( newModel, broadcastRoomUpdate updatedRoom newModel )

                    else
                        ( newModel, Cmd.none )


{-| Process a room during ping tick: increment missed pongs and handle timeouts
Returns: (updated room, list of timed out users with their clientIds, commands)
-}
processRoomPing :
    Model
    -> RoomId
    -> Room
    -> PingProcessState
    -> PingProcessState
processRoomPing model roomId room state =
    let
        -- Increment missed pongs for all participants (connected or disconnected)
        -- Hidden tabs get a long grace period before they are removed.
        -- Visible/disconnected participants keep the existing short timeout behavior.
        ( updatedParticipants, newTimedOut, hasStatusChange ) =
            Dict.foldl
                (\cId participant ( pAcc, tAcc, statusChanged ) ->
                    if participant.tabHidden then
                        let
                            newHiddenPingTicks =
                                participant.hiddenPingTicks + 1
                        in
                        if newHiddenPingTicks >= hiddenTabGraceTicks then
                            -- Hidden too long - remove them using the normal timeout flow
                            ( pAcc, ( cId, participant.name ) :: tAcc, True )

                        else
                            ( Dict.insert cId { participant | hiddenPingTicks = newHiddenPingTicks, missedPongs = 0 } pAcc
                            , tAcc
                            , statusChanged
                            )

                    else
                        let
                            newMissedPongs =
                                participant.missedPongs + 1

                            -- Only broadcast when entering critical state (1->2)
                            -- 0->1 is silent, timeout (2->3) handled separately
                            crossedThreshold =
                                participant.missedPongs == 1 && newMissedPongs == 2
                        in
                        if newMissedPongs >= 3 then
                            -- User timed out - remove them
                            ( pAcc, ( cId, participant.name ) :: tAcc, True )

                        else
                            -- Keep participant, increment counter
                            ( Dict.insert cId { participant | missedPongs = newMissedPongs } pAcc
                            , tAcc
                            , statusChanged || crossedThreshold
                            )
                )
                ( Dict.empty, [], False )
                room.participants

        updatedRoom =
            { room | participants = updatedParticipants }

        -- Check if room should be cleaned up
        connectedCount =
            Dict.filter (\_ p -> p.isConnected) updatedParticipants
                |> Dict.size

        -- If room is empty, don't add it back
        ( finalRooms, roomExists ) =
            if connectedCount == 0 && Dict.isEmpty updatedParticipants then
                ( state.rooms, False )

            else
                ( Dict.insert roomId updatedRoom state.rooms, True )

        -- Notify remaining participants about timed out users
        notifyCmds =
            if List.isEmpty newTimedOut then
                []

            else
                let
                    remainingClientIds =
                        Dict.keys updatedParticipants
                in
                List.concatMap
                    (\( _, timedOutName ) ->
                        List.map
                            (\cId -> sendToFrontend cId (UserTimedOut timedOutName))
                            remainingClientIds
                    )
                    newTimedOut

        -- Only broadcast room update if:
        -- 1. Room still exists AND
        -- 2. Either someone timed out OR a status crossed a UI threshold
        broadcastCmds =
            if roomExists && (not (List.isEmpty newTimedOut) || hasStatusChange) then
                Dict.keys updatedParticipants
                    |> List.map (\cId -> sendToFrontend cId (RoomUpdated updatedRoom))

            else
                []
    in
    { rooms = finalRooms
    , timedOutUsers = state.timedOutUsers ++ newTimedOut
    , cmds = state.cmds ++ notifyCmds ++ broadcastCmds
    , closedRoomIds =
        if roomExists then
            state.closedRoomIds

        else
            roomId :: state.closedRoomIds
    }



-- =============================================================================
-- Stats Helpers
-- =============================================================================


buildStats : StatsFilters -> Model -> Stats
buildStats filters model =
    { filters = filters
    , live = buildLiveStats model
    , recent = buildHistoricalStats filters (Just filters.recentDays) model
    , allTime = buildHistoricalStats filters Nothing model
    }


buildLiveStats : Model -> LiveStats
buildLiveStats model =
    let
        allParticipants =
            model.rooms
                |> Dict.values
                |> List.concatMap (\room -> Dict.values room.participants)
    in
    { activeRooms = Dict.size model.rooms
    , connectedUsers = Dict.size model.clientToRoom
    , awayUsers =
        allParticipants
            |> List.filter (\participant -> participant.isConnected && participant.tabHidden)
            |> List.length
    , roomsVoting =
        model.rooms
            |> Dict.values
            |> List.filter (\room -> not room.votesRevealed)
            |> List.length
    , roomsRevealed =
        model.rooms
            |> Dict.values
            |> List.filter .votesRevealed
            |> List.length
    }


buildHistoricalStats : StatsFilters -> Maybe Int -> Model -> HistoricalStats
buildHistoricalStats filters maybeWindowDays model =
    let
        sessions =
            model.completedSessionsByDay
                |> Dict.values
                |> List.concat
                |> List.filter (sessionIsInWindow model.currentDayIndex maybeWindowDays)
                |> List.filter (sessionPassesFilters filters)

        sessionCount =
            List.length sessions

        completedSessionCount =
            sessions
                |> List.filter .reachedReveal
                |> List.length

        totalVotes =
            sessions
                |> List.map .totalVotes
                |> List.sum

        totalParticipants =
            sessions
                |> List.map .participantCount
                |> List.sum

        aggregatedCardCounts =
            sessions
                |> List.foldl
                    (\sessionStats acc ->
                        Dict.foldl
                            (\voteName count innerAcc -> Dict.update voteName (incrementMaybe count) innerAcc)
                            acc
                            sessionStats.cardCounts
                    )
                    Dict.empty

        totalCardVotes =
            aggregatedCardCounts
                |> Dict.values
                |> List.sum
    in
    { windowDays = maybeWindowDays
    , sessionCount = sessionCount
    , completedSessionCount = completedSessionCount
    , totalVotes = totalVotes
    , averageParticipants =
        if sessionCount == 0 then
            0

        else
            toFloat totalParticipants / toFloat sessionCount
    , averageVotes =
        if sessionCount == 0 then
            0

        else
            toFloat totalVotes / toFloat sessionCount
    , revealRate =
        if sessionCount == 0 then
            0

        else
            (toFloat completedSessionCount / toFloat sessionCount) * 100
    , cardPercentages =
        voteDisplayOrder
            |> List.filterMap
                (\( voteName, label ) ->
                    Dict.get voteName aggregatedCardCounts
                        |> Maybe.map
                            (\count ->
                                { label = label
                                , voteCount = count
                                , percentage =
                                    if totalCardVotes == 0 then
                                        0

                                    else
                                        (toFloat count / toFloat totalCardVotes) * 100
                                }
                            )
                )
    , roomSizeBuckets = buildRoomSizeBuckets sessions
    , sessionsByDay = buildDayCounts sessions
    }


sessionIsInWindow : Int -> Maybe Int -> SessionStats -> Bool
sessionIsInWindow currentDayIndex maybeWindowDays sessionStats =
    case maybeWindowDays of
        Nothing ->
            True

        Just windowDays ->
            let
                daysAgo =
                    currentDayIndex - sessionStats.startedDayIndex
            in
            daysAgo >= 0 && daysAgo < windowDays


sessionPassesFilters : StatsFilters -> SessionStats -> Bool
sessionPassesFilters filters sessionStats =
    sessionStats.participantCount >= filters.minParticipants
        && sessionStats.totalVotes >= filters.minVotes


voteDisplayOrder : List ( String, String )
voteDisplayOrder =
    [ ( "0", "0" )
    , ( "1", "1" )
    , ( "half", "1/2" )
    , ( "2", "2" )
    , ( "3", "3" )
    , ( "5", "5" )
    , ( "8", "8" )
    , ( "13", "13" )
    , ( "21", "21" )
    , ( "100", "100" )
    , ( "question", "?" )
    , ( "coffee", "Coffee" )
    ]


buildRoomSizeBuckets : List SessionStats -> List RoomSizeBucket
buildRoomSizeBuckets sessions =
    let
        bucketCounts =
            sessions
                |> List.foldl
                    (\sessionStats acc -> Dict.update (roomSizeBucketLabel sessionStats.participantCount) (incrementMaybe 1) acc)
                    Dict.empty
    in
    [ "1", "2-3", "4-6", "7+" ]
        |> List.map (\label -> { label = label, sessionCount = Maybe.withDefault 0 (Dict.get label bucketCounts) })


roomSizeBucketLabel : Int -> String
roomSizeBucketLabel participantCount =
    if participantCount <= 1 then
        "1"

    else if participantCount <= 3 then
        "2-3"

    else if participantCount <= 6 then
        "4-6"

    else
        "7+"


buildDayCounts : List SessionStats -> List DayCount
buildDayCounts sessions =
    sessions
        |> List.foldl
            (\sessionStats acc -> Dict.update sessionStats.startedDayKey (incrementMaybe 1) acc)
            Dict.empty
        |> Dict.toList
        |> List.sortBy Tuple.first
        |> List.map (\( dayKey, sessionCount ) -> { dayKey = dayKey, sessionCount = sessionCount })


broadcastStatsUpdate : Model -> Cmd BackendMsg
broadcastStatsUpdate model =
    if Dict.isEmpty model.statsViewers then
        Cmd.none

    else
        Dict.toList model.statsViewers
            |> List.map (\( cId, filters ) -> sendToFrontend cId (StatsData (buildStats filters model)))
            |> Cmd.batch

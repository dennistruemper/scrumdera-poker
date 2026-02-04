module Backend exposing (..)

import Dict exposing (Dict)
import Env
import Lamdera exposing (ClientId, SessionId, sendToFrontend)
import Time
import Types exposing (..)


type alias Model =
    BackendModel


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
      }
    , Cmd.none
    )


subscriptions : Model -> Sub BackendMsg
subscriptions _ =
    Sub.batch
        [ Lamdera.onConnect ClientConnected
        , Lamdera.onDisconnect ClientDisconnected
        , Time.every 5000 (\_ -> PingTick)
        ]


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        ClientConnected _ clientId ->
            -- Client connected, nothing to do until they join a room
            ( model, Cmd.none )

        ClientDisconnected _ clientId ->
            -- Mark participant as disconnected and check if room should be cleaned up
            case Dict.get clientId model.clientToRoom of
                Nothing ->
                    ( model, Cmd.none )

                Just roomId ->
                    let
                        newModel =
                            { model | clientToRoom = Dict.remove clientId model.clientToRoom }

                        updatedRooms =
                            Dict.update roomId
                                (Maybe.map (markParticipantDisconnected clientId))
                                newModel.rooms

                        finalModel =
                            { newModel | rooms = updatedRooms }

                        -- Check if room should be removed (no connected participants)
                        ( cleanedModel, roomClosed ) =
                            cleanupRoomIfEmpty roomId finalModel
                    in
                    if roomClosed then
                        -- Notify remaining clients that room is closed
                        ( cleanedModel, Cmd.none )

                    else
                        -- Broadcast updated room state to remaining participants
                        case Dict.get roomId cleanedModel.rooms of
                            Just room ->
                                ( cleanedModel, broadcastRoomUpdate room cleanedModel )

                            Nothing ->
                                ( cleanedModel, Cmd.none )

        PingTick ->
            -- Increment missed pongs for all participants and check for timeouts
            let
                ( updatedRooms, timedOutUsers, cmds ) =
                    Dict.foldl
                        (processRoomPing model)
                        ( Dict.empty, [], [] )
                        model.rooms

                -- Remove timed out users from clientToRoom
                updatedClientToRoom =
                    List.foldl
                        (\( clientId, _ ) acc -> Dict.remove clientId acc)
                        model.clientToRoom
                        timedOutUsers

                newModel =
                    { model
                        | rooms = updatedRooms
                        , clientToRoom = updatedClientToRoom
                    }

                -- Send pings to all connected clients
                pingCmds =
                    Dict.keys model.clientToRoom
                        |> List.map (\cId -> sendToFrontend cId Ping)
            in
            ( newModel, Cmd.batch (cmds ++ pingCmds) )

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
                    }

                room =
                    { id = roomId
                    , pin = pin
                    , name = roomName
                    , participants = Dict.singleton clientId participant
                    , votesRevealed = False
                    }

                newModel =
                    { model
                        | rooms = Dict.insert roomId room model.rooms
                        , clientToRoom = Dict.insert clientId roomId model.clientToRoom
                    }
            in
            ( newModel
            , sendToFrontend clientId (RoomCreated room clientId)
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
                                }

                            updatedRoom =
                                { room | participants = Dict.insert clientId participant room.participants }

                            newModel =
                                { model
                                    | rooms = Dict.insert roomId updatedRoom model.rooms
                                    , clientToRoom = Dict.insert clientId roomId model.clientToRoom
                                }
                        in
                        ( newModel
                        , Cmd.batch
                            [ sendToFrontend clientId (RoomJoined updatedRoom clientId)
                            , broadcastRoomUpdate updatedRoom newModel
                            ]
                        )

        SubmitVote vote ->
            updateRoomForClient clientId model <|
                \room ->
                    let
                        updatedRoom =
                            { room
                                | participants =
                                    Dict.update clientId
                                        (Maybe.map (\p -> { p | vote = Just vote }))
                                        room.participants
                            }

                        -- Check for auto-reveal
                        finalRoom =
                            checkAutoReveal updatedRoom
                    in
                    finalRoom

        ClearMyVote ->
            updateRoomForClient clientId model <|
                \room ->
                    { room
                        | participants =
                            Dict.update clientId
                                (Maybe.map (\p -> { p | vote = Nothing }))
                                room.participants
                    }

        RevealVotes ->
            updateRoomForClient clientId model <|
                \room -> { room | votesRevealed = True }

        ResetVotes ->
            updateRoomForClient clientId model <|
                \room ->
                    { room
                        | votesRevealed = False
                        , participants =
                            Dict.map (\_ p -> { p | vote = Nothing }) room.participants
                    }

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
                    in
                    case Dict.get roomId cleanedModel.rooms of
                        Just room ->
                            ( cleanedModel, broadcastRoomUpdate room cleanedModel )

                        Nothing ->
                            ( cleanedModel, Cmd.none )

        RequestAdminData ->
            case Env.mode of
                Env.Development ->
                    let
                        stats =
                            { totalRooms = Dict.size model.rooms
                            , totalConnectedClients = Dict.size model.clientToRoom
                            , rooms =
                                Dict.values model.rooms
                                    |> List.map
                                        (\room ->
                                            { id = room.id
                                            , name = room.name
                                            , pin = room.pin
                                            , participantCount = Dict.size room.participants
                                            , connectedCount =
                                                Dict.filter (\_ p -> p.isConnected) room.participants
                                                    |> Dict.size
                                            , participants =
                                                Dict.values room.participants
                                                    |> List.map
                                                        (\p ->
                                                            { name = p.name
                                                            , isConnected = p.isConnected
                                                            , missedPongs = p.missedPongs
                                                            , hasVoted = p.vote /= Nothing
                                                            }
                                                        )
                                            , votesRevealed = room.votesRevealed
                                            }
                                        )
                            }
                    in
                    ( model, sendToFrontend clientId (AdminData stats) )

                Env.Production ->
                    ( model, Cmd.none )

        Pong ->
            -- Reset missed pongs counter for this client
            case Dict.get clientId model.clientToRoom of
                Nothing ->
                    ( model, Cmd.none )

                Just roomId ->
                    case Dict.get roomId model.rooms of
                        Nothing ->
                            ( model, Cmd.none )

                        Just room ->
                            -- Check if participant was in critical state (missedPongs >= 2)
                            let
                                wasCritical =
                                    Dict.get clientId room.participants
                                        |> Maybe.map (\p -> p.missedPongs >= 2)
                                        |> Maybe.withDefault False

                                updatedRoom =
                                    resetMissedPongs clientId room

                                newModel =
                                    { model | rooms = Dict.insert roomId updatedRoom model.rooms }
                            in
                            -- Only broadcast if clearing critical status (visible UI change)
                            if wasCritical then
                                ( newModel, broadcastRoomUpdate updatedRoom newModel )

                            else
                                ( newModel, Cmd.none )



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
                    ( newModel, broadcastRoomUpdate updatedRoom newModel )


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
                (Maybe.map (\p -> { p | isConnected = False }))
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
                ( { model | rooms = Dict.remove roomId model.rooms }, True )

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


{-| Process a room during ping tick: increment missed pongs and handle timeouts
Returns: (updated room, list of timed out users with their clientIds, commands)
-}
processRoomPing :
    Model
    -> RoomId
    -> Room
    -> ( Dict RoomId Room, List ( ClientId, String ), List (Cmd BackendMsg) )
    -> ( Dict RoomId Room, List ( ClientId, String ), List (Cmd BackendMsg) )
processRoomPing model roomId room ( accRooms, accTimedOut, accCmds ) =
    let
        -- Increment missed pongs for all participants (connected or disconnected)
        -- Only track status changes that affect UI: 1->2 (critical warning)
        -- We don't broadcast for 0->1 to avoid flicker (pong usually arrives quickly)
        ( updatedParticipants, newTimedOut, hasStatusChange ) =
            Dict.foldl
                (\cId participant ( pAcc, tAcc, statusChanged ) ->
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
                ( accRooms, False )

            else
                ( Dict.insert roomId updatedRoom accRooms, True )

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
    ( finalRooms
    , accTimedOut ++ newTimedOut
    , accCmds ++ notifyCmds ++ broadcastCmds
    )

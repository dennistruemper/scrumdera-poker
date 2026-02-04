module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Lamdera exposing (ClientId)
import Set exposing (Set)
import Url exposing (Url)


-- =============================================================================
-- Core Types
-- =============================================================================


type alias RoomId =
    String


type alias Pin =
    String


type Vote
    = NumericVote Int
    | QuestionMark
    | CoffeeBreak


type alias Participant =
    { name : String
    , vote : Maybe Vote
    , isConnected : Bool
    , missedPongs : Int
    }


type ConnectionStatus
    = Connected
    | Unstable
    | Critical


type alias Room =
    { id : RoomId
    , pin : Pin
    , name : String
    , participants : Dict ClientId Participant
    , votesRevealed : Bool
    }


type alias Notification =
    { message : String
    , remainingPings : Int -- Will be decremented each ping, removed at 0
    }


type Page
    = HomePage
    | RoomPage RoomId (Maybe Pin)
    | StatsPage


type alias Stats =
    { totalRooms : Int
    , totalConnectedClients : Int
    , rooms : List RoomSummary
    }


type alias RoomSummary =
    { name : String
    , participantCount : Int
    , connectedCount : Int
    , participants : List ParticipantSummary
    , votesRevealed : Bool
    }


type alias ParticipantSummary =
    { isConnected : Bool
    , missedPongs : Int
    , hasVoted : Bool
    }


-- =============================================================================
-- Frontend Model
-- =============================================================================


type alias FrontendModel =
    { key : Key
    , page : Page
    , roomData : Maybe Room
    , userName : String
    , newRoomName : String
    , joinRoomId : String
    , joinRoomPin : String
    , error : Maybe String
    , myClientId : Maybe ClientId
    , clipboardFeedback : Maybe String
    , showShareLink : Bool
    , baseUrl : String
    , notifications : List Notification
    , stats : Maybe Stats
    }


-- =============================================================================
-- Backend Model
-- =============================================================================


type alias BackendModel =
    { rooms : Dict RoomId Room
    , clientToRoom : Dict ClientId RoomId
    , statsViewers : Set ClientId
    }


-- =============================================================================
-- Frontend Messages (UI interactions)
-- =============================================================================


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | SetUserName String
    | SetNewRoomName String
    | SetJoinRoomId String
    | SetJoinRoomPin String
    | CreateRoomClicked
    | JoinRoomClicked
    | CastVote Vote
    | ClearVote
    | RevealVotesClicked
    | ResetVotesClicked
    | CopyShareLink
    | ToggleShareLink
    | ClipboardResult { success : Bool, message : String }
    | ClearClipboardFeedback
    | RefreshStats
    | NoOpFrontendMsg


-- =============================================================================
-- ToBackend Messages (Frontend -> Backend)
-- =============================================================================


type ToBackend
    = CreateRoom String String -- roomName, userName
    | JoinRoom RoomId Pin String -- roomId, pin, userName
    | SubmitVote Vote
    | ClearMyVote
    | RevealVotes
    | ResetVotes
    | LeaveRoom
    | SubscribeToStats
    | UnsubscribeFromStats
    | Pong -- Response to ping


-- =============================================================================
-- Backend Messages (internal)
-- =============================================================================


type BackendMsg
    = ClientConnected Lamdera.SessionId ClientId
    | ClientDisconnected Lamdera.SessionId ClientId
    | PingTick
    | NoOpBackendMsg


-- =============================================================================
-- ToFrontend Messages (Backend -> Frontend)
-- =============================================================================


type ToFrontend
    = RoomCreated Room ClientId
    | RoomJoined Room ClientId
    | RoomUpdated Room
    | RoomNotFound
    | InvalidPin
    | RoomClosed
    | ErrorMessage String
    | StatsData Stats
    | Ping -- Heartbeat from server
    | UserTimedOut String -- userName who timed out

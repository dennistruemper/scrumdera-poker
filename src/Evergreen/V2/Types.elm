module Evergreen.V2.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Lamdera
import Set
import Url


type alias RoomId =
    String


type alias Pin =
    String


type Page
    = HomePage
    | RoomPage RoomId (Maybe Pin)
    | StatsPage


type Vote
    = NumericVote Int
    | HalfPoint
    | QuestionMark
    | CoffeeBreak


type alias Participant =
    { name : String
    , vote : Maybe Vote
    , isConnected : Bool
    , missedPongs : Int
    }


type alias Room =
    { id : RoomId
    , pin : Pin
    , name : String
    , participants : Dict.Dict Lamdera.ClientId Participant
    , votesRevealed : Bool
    }


type alias Notification =
    { message : String
    , remainingPings : Int
    }


type alias ParticipantSummary =
    { isConnected : Bool
    , missedPongs : Int
    , hasVoted : Bool
    }


type alias RoomSummary =
    { name : String
    , participantCount : Int
    , connectedCount : Int
    , participants : List ParticipantSummary
    , votesRevealed : Bool
    }


type alias Stats =
    { totalRooms : Int
    , totalConnectedClients : Int
    , rooms : List RoomSummary
    }


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , page : Page
    , roomData : Maybe Room
    , userName : String
    , newRoomName : String
    , joinRoomId : String
    , joinRoomPin : String
    , error : Maybe String
    , myClientId : Maybe Lamdera.ClientId
    , clipboardFeedback : Maybe String
    , showShareLink : Bool
    , baseUrl : String
    , notifications : List Notification
    , stats : Maybe Stats
    }


type alias BackendModel =
    { rooms : Dict.Dict RoomId Room
    , clientToRoom : Dict.Dict Lamdera.ClientId RoomId
    , statsViewers : Set.Set Lamdera.ClientId
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
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
    | ClipboardResult
        { success : Bool
        , message : String
        }
    | ClearClipboardFeedback
    | RefreshStats
    | NoOpFrontendMsg


type ToBackend
    = CreateRoom String String
    | JoinRoom RoomId Pin String
    | SubmitVote Vote
    | ClearMyVote
    | RevealVotes
    | ResetVotes
    | LeaveRoom
    | SubscribeToStats
    | UnsubscribeFromStats
    | Pong


type BackendMsg
    = ClientConnected Lamdera.SessionId Lamdera.ClientId
    | ClientDisconnected Lamdera.SessionId Lamdera.ClientId
    | PingTick
    | NoOpBackendMsg


type ToFrontend
    = RoomCreated Room Lamdera.ClientId
    | RoomJoined Room Lamdera.ClientId
    | RoomUpdated Room
    | RoomNotFound
    | InvalidPin
    | RoomClosed
    | ErrorMessage String
    | StatsData Stats
    | Ping
    | UserTimedOut String

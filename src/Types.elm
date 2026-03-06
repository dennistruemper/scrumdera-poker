module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Lamdera exposing (ClientId)
import Time
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
    | HalfPoint -- 1/2
    | QuestionMark
    | CoffeeBreak


type alias Participant =
    { name : String
    , vote : Maybe Vote
    , isConnected : Bool
    , missedPongs : Int
    , tabHidden : Bool
    , hiddenPingTicks : Int
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
    { filters : StatsFilters
    , live : LiveStats
    , recent : HistoricalStats
    , allTime : HistoricalStats
    }


type alias StatsFilters =
    { recentDays : Int
    , minParticipants : Int
    , minVotes : Int
    }


type alias LiveStats =
    { activeRooms : Int
    , connectedUsers : Int
    , awayUsers : Int
    , roomsVoting : Int
    , roomsRevealed : Int
    }


type alias HistoricalStats =
    { windowDays : Maybe Int
    , sessionCount : Int
    , completedSessionCount : Int
    , totalVotes : Int
    , averageParticipants : Float
    , averageVotes : Float
    , revealRate : Float
    , cardPercentages : List CardPercentage
    , roomSizeBuckets : List RoomSizeBucket
    , sessionsByDay : List DayCount
    }


type alias CardPercentage =
    { label : String
    , voteCount : Int
    , percentage : Float
    }


type alias RoomSizeBucket =
    { label : String
    , sessionCount : Int
    }


type alias DayCount =
    { dayKey : String
    , sessionCount : Int
    }


type alias SessionStats =
    { startedDayKey : String
    , startedDayIndex : Int
    , participantCount : Int
    , totalVotes : Int
    , reachedReveal : Bool
    , cardCounts : Dict String Int
    , currentRoundVotes : Dict ClientId Vote
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
    , statsFilters : StatsFilters
    }


-- =============================================================================
-- Backend Model
-- =============================================================================


type alias BackendModel =
    { rooms : Dict RoomId Room
    , clientToRoom : Dict ClientId RoomId
    , statsViewers : Dict ClientId StatsFilters
    , activeSessionStats : Dict RoomId SessionStats
    , completedSessionsByDay : Dict String (List SessionStats)
    , currentDayKey : String
    , currentDayIndex : Int
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
    | SetStatsRecentDays String
    | SetStatsMinParticipants String
    | SetStatsMinVotes String
    | TabBecameVisible
    | TabBecameHidden
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
    | SubscribeToStats StatsFilters
    | UpdateStatsFilters StatsFilters
    | UnsubscribeFromStats
    | Pong -- Response to ping
    | VisibilityPong -- User returned to the tab
    | TabHidden -- User switched to another tab


-- =============================================================================
-- Backend Messages (internal)
-- =============================================================================


type BackendMsg
    = ClientConnected Lamdera.SessionId ClientId
    | ClientDisconnected Lamdera.SessionId ClientId
    | InitializeCurrentDay Time.Posix
    | PingTick Time.Posix
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

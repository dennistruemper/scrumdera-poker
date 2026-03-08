module Evergreen.V5.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Lamdera
import Time
import Url


type alias RoomId =
    String


type alias Pin =
    String


type Page
    = HomePage
    | RoomPage RoomId (Maybe Pin)
    | StatsPage


type ThemePreference
    = UseSystemTheme
    | UseLightTheme
    | UseDarkTheme


type Theme
    = LightTheme
    | DarkTheme


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
    , tabHidden : Bool
    , hiddenPingTicks : Int
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


type alias Stats =
    { filters : StatsFilters
    , live : LiveStats
    , recent : HistoricalStats
    , allTime : HistoricalStats
    }


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , page : Page
    , themePreference : ThemePreference
    , systemTheme : Theme
    , activeTheme : Theme
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
    , statsFilters : StatsFilters
    }


type alias SessionStats =
    { startedDayKey : String
    , startedDayIndex : Int
    , participantCount : Int
    , totalVotes : Int
    , reachedReveal : Bool
    , cardCounts : Dict.Dict String Int
    , currentRoundVotes : Dict.Dict Lamdera.ClientId Vote
    }


type alias BackendModel =
    { rooms : Dict.Dict RoomId Room
    , clientToRoom : Dict.Dict Lamdera.ClientId RoomId
    , statsViewers : Dict.Dict Lamdera.ClientId StatsFilters
    , activeSessionStats : Dict.Dict RoomId SessionStats
    , completedSessionsByDay : Dict.Dict String (List SessionStats)
    , currentDayKey : String
    , currentDayIndex : Int
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | SetThemePreference ThemePreference
    | ThemeStateReceived String Bool
    | SystemThemeChanged Bool
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
    | SetStatsRecentDays String
    | SetStatsMinParticipants String
    | SetStatsMinVotes String
    | TabBecameVisible
    | TabBecameHidden
    | NoOpFrontendMsg


type ToBackend
    = CreateRoom String String
    | JoinRoom RoomId Pin String
    | SubmitVote Vote
    | ClearMyVote
    | RevealVotes
    | ResetVotes
    | LeaveRoom
    | SubscribeToStats StatsFilters
    | UpdateStatsFilters StatsFilters
    | UnsubscribeFromStats
    | Pong
    | VisibilityPong
    | TabHidden


type BackendMsg
    = ClientConnected Lamdera.SessionId Lamdera.ClientId
    | ClientDisconnected Lamdera.SessionId Lamdera.ClientId
    | InitializeCurrentDay Time.Posix
    | PingTick Time.Posix
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

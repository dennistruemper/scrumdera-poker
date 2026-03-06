module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Dict
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
import Lamdera
import Ports
import Process
import Task
import Types exposing (..)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>))
import Url.Parser.Query as Query


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : Model -> Sub FrontendMsg
subscriptions _ =
    Ports.clipboardResult ClipboardResult


init : Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    let
        page =
            parseUrl url

        -- Pre-fill join fields if we're on a room page with PIN
        ( joinRoomId, joinRoomPin ) =
            case page of
                RoomPage roomId (Just pin) ->
                    ( roomId, pin )

                _ ->
                    ( "", "" )

        -- Build base URL from current location
        baseUrl =
            let
                protocol =
                    case url.protocol of
                        Url.Http ->
                            "http://"

                        Url.Https ->
                            "https://"

                port_ =
                    case url.port_ of
                        Just p ->
                            ":" ++ String.fromInt p

                        Nothing ->
                            ""
            in
            protocol ++ url.host ++ port_
    in
    ( { key = key
      , page = page
      , roomData = Nothing
      , userName = ""
      , newRoomName = ""
      , joinRoomId = joinRoomId
      , joinRoomPin = joinRoomPin
      , error = Nothing
      , myClientId = Nothing
      , clipboardFeedback = Nothing
      , showShareLink = False
      , baseUrl = baseUrl
      , notifications = []
      , stats = Nothing
      }
    , case page of
        StatsPage ->
            Lamdera.sendToBackend SubscribeToStats

        _ ->
            Cmd.none
    )



-- =============================================================================
-- URL Parsing
-- =============================================================================


parseUrl : Url -> Page
parseUrl url =
    Parser.parse routeParser url
        |> Maybe.withDefault HomePage


routeParser : Parser.Parser (Page -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map HomePage Parser.top
        , Parser.map (\rid pin -> RoomPage rid pin) (Parser.s "room" </> Parser.string <?> Query.string "pin")
        , Parser.map StatsPage (Parser.s "stats")
        ]



-- =============================================================================
-- Update
-- =============================================================================


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                External url ->
                    ( model, Nav.load url )

        UrlChanged url ->
            let
                newPage =
                    parseUrl url

                -- Unsubscribe from stats if leaving stats page
                unsubscribeCmd =
                    if model.page == StatsPage && newPage /= StatsPage then
                        Lamdera.sendToBackend UnsubscribeFromStats

                    else
                        Cmd.none

                -- Subscribe to stats if entering stats page
                subscribeCmd =
                    if newPage == StatsPage then
                        Lamdera.sendToBackend SubscribeToStats

                    else
                        Cmd.none
            in
            ( { model | page = newPage, error = Nothing }
            , Cmd.batch [ unsubscribeCmd, subscribeCmd ]
            )

        SetUserName name ->
            ( { model | userName = name }, Cmd.none )

        SetNewRoomName name ->
            ( { model | newRoomName = name }, Cmd.none )

        SetJoinRoomId id ->
            ( { model | joinRoomId = id }, Cmd.none )

        SetJoinRoomPin pin ->
            ( { model | joinRoomPin = pin }, Cmd.none )

        CreateRoomClicked ->
            if String.isEmpty model.userName then
                ( { model | error = Just "Please enter your name" }, Cmd.none )

            else if String.isEmpty model.newRoomName then
                ( { model | error = Just "Please enter a room name" }, Cmd.none )

            else
                ( { model | error = Nothing }
                , Lamdera.sendToBackend (CreateRoom model.newRoomName model.userName)
                )

        JoinRoomClicked ->
            if String.isEmpty model.userName then
                ( { model | error = Just "Please enter your name" }, Cmd.none )

            else if String.isEmpty model.joinRoomId then
                ( { model | error = Just "Please enter a room ID" }, Cmd.none )

            else if String.isEmpty model.joinRoomPin then
                ( { model | error = Just "Please enter the room PIN" }, Cmd.none )

            else
                ( { model | error = Nothing }
                , Lamdera.sendToBackend (JoinRoom model.joinRoomId model.joinRoomPin model.userName)
                )

        CastVote vote ->
            ( model, Lamdera.sendToBackend (SubmitVote vote) )

        ClearVote ->
            ( model, Lamdera.sendToBackend ClearMyVote )

        RevealVotesClicked ->
            ( model, Lamdera.sendToBackend RevealVotes )

        ResetVotesClicked ->
            ( model, Lamdera.sendToBackend ResetVotes )

        CopyShareLink ->
            case model.roomData of
                Just room ->
                    let
                        shareUrl =
                            model.baseUrl ++ "/room/" ++ room.id ++ "?pin=" ++ room.pin
                    in
                    ( model, Ports.copyToClipboard shareUrl )

                Nothing ->
                    ( model, Cmd.none )

        ToggleShareLink ->
            ( { model | showShareLink = not model.showShareLink }, Cmd.none )

        ClipboardResult result ->
            ( { model | clipboardFeedback = Just result.message }
            , Process.sleep 3000 |> Task.perform (\_ -> ClearClipboardFeedback)
            )

        ClearClipboardFeedback ->
            ( { model | clipboardFeedback = Nothing }, Cmd.none )

        RefreshStats ->
            ( model, Lamdera.sendToBackend SubscribeToStats )

        NoOpFrontendMsg ->
            ( model, Cmd.none )



-- =============================================================================
-- Update From Backend
-- =============================================================================


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        RoomCreated room clientId ->
            ( { model
                | roomData = Just room
                , myClientId = Just clientId
                , page = RoomPage room.id (Just room.pin)
              }
            , Nav.pushUrl model.key ("/room/" ++ room.id ++ "?pin=" ++ room.pin)
            )

        RoomJoined room clientId ->
            ( { model
                | roomData = Just room
                , myClientId = Just clientId
                , page = RoomPage room.id (Just room.pin)
              }
            , Cmd.none
            )

        RoomUpdated room ->
            ( { model | roomData = Just room }, Cmd.none )

        RoomNotFound ->
            ( { model | error = Just "Room not found" }, Cmd.none )

        InvalidPin ->
            ( { model | error = Just "Invalid PIN" }, Cmd.none )

        RoomClosed ->
            ( { model | roomData = Nothing, error = Just "Room was closed" }
            , Nav.pushUrl model.key "/"
            )

        ErrorMessage err ->
            ( { model | error = Just err }, Cmd.none )

        StatsData stats ->
            ( { model | stats = Just stats }, Cmd.none )

        Ping ->
            -- Respond with Pong and decrement notification counters
            let
                updatedNotifications =
                    model.notifications
                        |> List.map (\n -> { n | remainingPings = n.remainingPings - 1 })
                        |> List.filter (\n -> n.remainingPings > 0)
            in
            ( { model | notifications = updatedNotifications }
            , Lamdera.sendToBackend Pong
            )

        UserTimedOut userName ->
            -- Add notification about user timeout
            let
                notification =
                    { message = userName ++ " disconnected (connection lost)"
                    , remainingPings = 3
                    }
            in
            ( { model | notifications = notification :: model.notifications }
            , Cmd.none
            )



-- =============================================================================
-- View
-- =============================================================================


view : Model -> Browser.Document FrontendMsg
view model =
    { title = "Scrumdera Poker"
    , body =
        [ node "link" [ Attr.rel "stylesheet", Attr.href "/styles.css" ] []
        , case model.page of
            HomePage ->
                viewHomePage model

            RoomPage roomId maybePin ->
                viewRoomPage model roomId maybePin

            StatsPage ->
                viewStatsPage model
        ]
    }


viewHomePage : Model -> Html FrontendMsg
viewHomePage model =
    div [ Attr.class "container-narrow" ]
        [ h1 [ Attr.class "text-center mb-2" ] [ text "Scrumdera Poker" ]
        , p [ Attr.class "text-center text-muted mb-8" ]
            [ text "Simple scrum poker for agile teams" ]

        -- Error display
        , viewError model.error

        -- Username input
        , div [ Attr.class "form-group mb-6" ]
            [ label [ Attr.class "form-label" ] [ text "Your Name" ]
            , input
                [ Attr.type_ "text"
                , Attr.class "form-input"
                , Attr.placeholder "Enter your name"
                , Attr.value model.userName
                , onInput SetUserName
                ]
                []
            ]

        -- Create room section
        , div [ Attr.class "card" ]
            [ h2 [] [ text "Create a Room" ]
            , div [ Attr.class "form-group" ]
                [ input
                    [ Attr.type_ "text"
                    , Attr.class "form-input"
                    , Attr.placeholder "Room name (e.g., Sprint 42)"
                    , Attr.value model.newRoomName
                    , onInput SetNewRoomName
                    ]
                    []
                ]
            , button
                [ Attr.class "btn btn-primary"
                , onClick CreateRoomClicked
                ]
                [ text "Create Room" ]
            ]

        -- Join room section
        , div [ Attr.class "card" ]
            [ h2 [] [ text "Join a Room" ]
            , div [ Attr.class "form-group" ]
                [ input
                    [ Attr.type_ "text"
                    , Attr.class "form-input"
                    , Attr.placeholder "Room ID"
                    , Attr.value model.joinRoomId
                    , onInput SetJoinRoomId
                    ]
                    []
                ]
            , div [ Attr.class "form-group" ]
                [ input
                    [ Attr.type_ "text"
                    , Attr.class "form-input"
                    , Attr.placeholder "PIN"
                    , Attr.value model.joinRoomPin
                    , onInput SetJoinRoomPin
                    ]
                    []
                ]
            , button
                [ Attr.class "btn btn-success"
                , onClick JoinRoomClicked
                ]
                [ text "Join Room" ]
            ]

        -- Stats link
        , div [ Attr.class "mt-8 text-center" ]
            [ a [ Attr.href "/stats", Attr.class "text-muted text-small" ]
                [ text "Live Stats" ]
            ]
        ]


viewRoomPage : Model -> RoomId -> Maybe Pin -> Html FrontendMsg
viewRoomPage model roomId maybePin =
    case model.roomData of
        Nothing ->
            div [ Attr.class "container-narrow text-center" ]
                [ h2 [] [ text "Join Room" ]

                -- Error display
                , viewError model.error
                , case maybePin of
                    Just _ ->
                        -- Show join form
                        div [ Attr.class "mt-4" ]
                            [ div [ Attr.class "form-group" ]
                                [ input
                                    [ Attr.type_ "text"
                                    , Attr.class "form-input"
                                    , Attr.placeholder "Your name"
                                    , Attr.value model.userName
                                    , onInput SetUserName
                                    ]
                                    []
                                ]
                            , button
                                [ Attr.class "btn btn-primary"
                                , onClick JoinRoomClicked
                                ]
                                [ text "Join Room" ]
                            , p [ Attr.class "text-muted text-small mt-4" ]
                                [ text ("Room: " ++ roomId) ]
                            ]

                    Nothing ->
                        p [ Attr.class "alert alert-error mt-4" ] [ text "Missing PIN in URL" ]
                ]

        Just room ->
            viewRoom model room


viewRoom : Model -> Room -> Html FrontendMsg
viewRoom model room =
    let
        shareUrl =
            model.baseUrl ++ "/room/" ++ room.id ++ "?pin=" ++ room.pin
    in
    div [ Attr.class "container" ]
        [ -- Notifications
          viewNotifications model.notifications

        -- Header
        , div [ Attr.class "room-header" ]
            [ h1 [] [ text room.name ]
            , p [ Attr.class "room-info" ]
                [ text ("Room: " ++ room.id ++ " | PIN: " ++ room.pin) ]
            , div [ Attr.class "share-link-container" ]
                [ button
                    [ Attr.class "btn btn-secondary btn-small"
                    , onClick ToggleShareLink
                    ]
                    [ text
                        (if model.showShareLink then
                            "Hide Share Link"

                         else
                            "Show Share Link"
                        )
                    ]
                , if model.showShareLink then
                    div [ Attr.class "share-link-box" ]
                        [ input
                            [ Attr.type_ "text"
                            , Attr.class "form-input share-link-input"
                            , Attr.value shareUrl
                            , Attr.readonly True
                            ]
                            []
                        , button
                            [ Attr.class "btn btn-primary btn-small"
                            , onClick CopyShareLink
                            ]
                            [ text "Copy" ]
                        ]

                  else
                    text ""
                , case model.clipboardFeedback of
                    Just feedback ->
                        span [ Attr.class "clipboard-feedback" ] [ text feedback ]

                    Nothing ->
                        text ""
                ]
            ]

        -- Voting cards
        , div [ Attr.class "mb-8" ]
            [ h2 [ Attr.class "text-center mb-4" ] [ text "Your Vote" ]
            , div [ Attr.class "vote-cards" ]
                (List.map (viewVoteCard model room) voteOptions)
            ]

        -- Action buttons
        , div [ Attr.class "action-buttons" ]
            [ button
                [ Attr.class "btn btn-purple"
                , onClick RevealVotesClicked
                ]
                [ text "Reveal Votes" ]
            , button
                [ Attr.class "btn btn-secondary"
                , onClick ResetVotesClicked
                ]
                [ text "Reset" ]
            ]

        -- Participants
        , div [ Attr.class "card", Attr.style "max-width" "42rem", Attr.style "margin-left" "auto", Attr.style "margin-right" "auto" ]
            [ h2 [ Attr.class "mb-4" ] [ text "Participants" ]
            , ul [ Attr.class "participants-list" ]
                (room.participants
                    |> Dict.toList
                    |> List.map (viewParticipant room.votesRevealed)
                )
            ]

        -- Results (when revealed)
        , if room.votesRevealed then
            viewResults room

          else
            text ""
        ]


viewError : Maybe String -> Html msg
viewError maybeError =
    case maybeError of
        Just err ->
            div [ Attr.class "alert alert-error" ] [ text err ]

        Nothing ->
            text ""


viewNotifications : List Notification -> Html msg
viewNotifications notifications =
    if List.isEmpty notifications then
        text ""

    else
        div [ Attr.class "notifications-container" ]
            (List.map viewNotification notifications)


viewNotification : Notification -> Html msg
viewNotification notification =
    div [ Attr.class "notification" ]
        [ text notification.message ]


voteOptions : List Vote
voteOptions =
    [ NumericVote 0
    , HalfPoint
    , NumericVote 1
    , NumericVote 2
    , NumericVote 3
    , NumericVote 5
    , NumericVote 8
    , NumericVote 13
    , NumericVote 21
    , NumericVote 100
    , QuestionMark
    , CoffeeBreak
    ]


viewVoteCard : Model -> Room -> Vote -> Html FrontendMsg
viewVoteCard model room vote =
    let
        myVote =
            model.myClientId
                |> Maybe.andThen (\cId -> Dict.get cId room.participants)
                |> Maybe.andThen .vote

        isSelected =
            myVote == Just vote

        className =
            if isSelected then
                "vote-card selected"

            else
                "vote-card"

        cornerText =
            voteToString vote

        centerContent =
            case vote of
                CoffeeBreak ->
                    span [ Attr.class "card-emoji" ] [ text "☕" ]

                QuestionMark ->
                    img [ Attr.src "/cards/mystery.svg", Attr.alt "?" ] []

                HalfPoint ->
                    img [ Attr.src "/cards/half.svg", Attr.alt "½" ] []

                NumericVote n ->
                    img [ Attr.src ("/cards/" ++ String.fromInt n ++ ".svg"), Attr.alt cornerText ] []
    in
    button
        [ Attr.class className
        , onClick (CastVote vote)
        ]
        [ span [ Attr.class "card-corner card-corner-top" ] [ text cornerText ]
        , div [ Attr.class "card-center" ] [ centerContent ]
        , span [ Attr.class "card-corner card-corner-bottom" ] [ text cornerText ]
        ]


voteToString : Vote -> String
voteToString vote =
    case vote of
        NumericVote n ->
            String.fromInt n

        HalfPoint ->
            "½"

        QuestionMark ->
            "?"

        CoffeeBreak ->
            "☕"


viewParticipant : Bool -> ( String, Participant ) -> Html FrontendMsg
viewParticipant revealed ( _, participant ) =
    let
        connectionStatus =
            if not participant.isConnected then
                "disconnected"

            else if participant.missedPongs >= 2 then
                "critical"

            else
                "connected"

        statusTitle =
            if not participant.isConnected then
                "Disconnected"

            else if participant.missedPongs >= 2 then
                "Connection critical - may disconnect soon"

            else
                "Connected"
    in
    li [ Attr.class "participant-item" ]
        [ div [ Attr.class "participant-info" ]
            [ span
                [ Attr.class ("status-dot " ++ connectionStatus)
                , Attr.title statusTitle
                ]
                []
            , span [] [ text participant.name ]
            , if participant.missedPongs >= 2 && participant.isConnected then
                span [ Attr.class "connection-warning critical" ]
                    [ text "⚠️" ]

              else
                text ""
            ]
        , span [ Attr.class "participant-vote" ]
            [ text
                (case participant.vote of
                    Nothing ->
                        "..."

                    Just vote ->
                        if revealed then
                            voteToString vote

                        else
                            "✓"
                )
            ]
        ]


viewResults : Room -> Html FrontendMsg
viewResults room =
    let
        numericVotes =
            room.participants
                |> Dict.values
                |> List.filterMap .vote
                |> List.filterMap
                    (\v ->
                        case v of
                            NumericVote n ->
                                Just (toFloat n)

                            HalfPoint ->
                                Just 0.5

                            _ ->
                                Nothing
                    )

        average =
            if List.isEmpty numericVotes then
                Nothing

            else
                Just (List.sum numericVotes / toFloat (List.length numericVotes))

        coffeeBreaks =
            room.participants
                |> Dict.values
                |> List.filterMap .vote
                |> List.filter ((==) CoffeeBreak)
                |> List.length
    in
    div [ Attr.class "results", Attr.style "max-width" "42rem", Attr.style "margin-left" "auto", Attr.style "margin-right" "auto" ]
        [ h2 [ Attr.class "mb-4" ] [ text "Results" ]
        , case average of
            Just avg ->
                p [ Attr.class "results-average" ]
                    [ text ("Average: " ++ String.fromFloat (toFloat (round (avg * 10)) / 10)) ]

            Nothing ->
                p [ Attr.class "text-muted" ] [ text "No numeric votes" ]
        , if coffeeBreaks > 0 then
            p [ Attr.class "results-break" ]
                [ text ("☕ " ++ String.fromInt coffeeBreaks ++ " vote(s) for a break") ]

          else
            text ""
        ]


viewStatsPage : Model -> Html FrontendMsg
viewStatsPage model =
    div [ Attr.class "container stats-container" ]
        [ div [ Attr.class "stats-header" ]
            [ h1 [] [ text "Live Stats" ]
            , div [ Attr.class "stats-actions" ]
                [ a [ Attr.href "/", Attr.class "btn btn-secondary btn-small" ]
                    [ text "← Home" ]
                ]
            ]
        , case model.stats of
            Nothing ->
                div [ Attr.class "text-center text-muted" ]
                    [ text "Loading..." ]

            Just stats ->
                div []
                    [ -- Stats overview
                      div [ Attr.class "stats-overview" ]
                        [ div [ Attr.class "stat-card" ]
                            [ div [ Attr.class "stat-value" ] [ text (String.fromInt stats.totalRooms) ]
                            , div [ Attr.class "stat-label" ] [ text "Active Rooms" ]
                            ]
                        , div [ Attr.class "stat-card" ]
                            [ div [ Attr.class "stat-value" ] [ text (String.fromInt stats.totalConnectedClients) ]
                            , div [ Attr.class "stat-label" ] [ text "Connected Users" ]
                            ]
                        ]

                    -- Rooms list
                    , if List.isEmpty stats.rooms then
                        div [ Attr.class "card text-center text-muted" ]
                            [ text "No active rooms" ]

                      else
                        div []
                            (List.map viewStatsRoom stats.rooms)
                    ]
        ]


viewStatsRoom : RoomSummary -> Html FrontendMsg
viewStatsRoom room =
    div [ Attr.class "stats-room card" ]
        [ div [ Attr.class "stats-room-header" ]
            [ h3 [] [ text room.name ]
            , div [ Attr.class "stats-room-meta" ]
                [ span
                    [ Attr.class
                        (if room.votesRevealed then
                            "stats-badge badge-revealed"

                         else
                            "stats-badge"
                        )
                    ]
                    [ text
                        (if room.votesRevealed then
                            "Votes Revealed"

                         else
                            "Voting"
                        )
                    ]
                ]
            ]
        , div [ Attr.class "stats-participants" ]
            [ h4 []
                [ text
                    ("Users: "
                        ++ String.fromInt room.connectedCount
                        ++ "/"
                        ++ String.fromInt room.participantCount
                        ++ " connected"
                    )
                ]
            , if List.isEmpty room.participants then
                p [ Attr.class "text-muted" ] [ text "No users" ]

              else
                ul [ Attr.class "stats-participant-list" ]
                    (List.indexedMap viewStatsParticipant room.participants)
            ]
        ]


viewStatsParticipant : Int -> ParticipantSummary -> Html FrontendMsg
viewStatsParticipant index participant =
    let
        connectionClass =
            if not participant.isConnected then
                "disconnected"

            else if participant.missedPongs >= 2 then
                "critical"

            else
                "connected"
    in
    li [ Attr.class "stats-participant-item" ]
        [ span [ Attr.class ("status-dot " ++ connectionClass) ] []
        , span [ Attr.class "stats-participant-name" ] [ text ("User " ++ String.fromInt (index + 1)) ]
        , span
            [ Attr.class
                (if participant.hasVoted then
                    "stats-vote-status voted"

                 else
                    "stats-vote-status"
                )
            ]
            [ text
                (if participant.hasVoted then
                    "✓ Voted"

                 else
                    "..."
                )
            ]
        ]

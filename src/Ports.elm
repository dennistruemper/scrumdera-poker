port module Ports exposing
    ( ClipboardResult
    , clipboardResult
    , copyToClipboard
    , forgetPlayerName
    , rememberPlayerName
    , requestSavedNames
    , requestThemeState
    , saveThemePreference
    , savedNamesReceived
    , systemThemeChanged
    , themeState
    )

-- Outgoing port: send text to copy to clipboard


port copyToClipboard : String -> Cmd msg



-- Incoming port: receive result of clipboard operation


type alias ClipboardResult =
    { success : Bool
    , message : String
    }


port clipboardResult : (ClipboardResult -> msg) -> Sub msg


-- Theme ports


port requestThemeState : () -> Cmd msg


port saveThemePreference : String -> Cmd msg


port themeState : ({ preference : String, systemDark : Bool } -> msg) -> Sub msg


port systemThemeChanged : (Bool -> msg) -> Sub msg


-- Saved player names (IndexedDB)


port requestSavedNames : () -> Cmd msg


port rememberPlayerName : String -> Cmd msg


port forgetPlayerName : String -> Cmd msg


port savedNamesReceived : (List { name : String, lastUsed : Int } -> msg) -> Sub msg

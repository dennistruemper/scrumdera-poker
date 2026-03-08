port module Ports exposing
    ( ClipboardResult
    , clipboardResult
    , copyToClipboard
    , requestThemeState
    , saveThemePreference
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

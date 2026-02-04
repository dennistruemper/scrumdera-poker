port module Ports exposing (ClipboardResult, clipboardResult, copyToClipboard)

-- Outgoing port: send text to copy to clipboard


port copyToClipboard : String -> Cmd msg



-- Incoming port: receive result of clipboard operation


type alias ClipboardResult =
    { success : Bool
    , message : String
    }


port clipboardResult : (ClipboardResult -> msg) -> Sub msg

module Main.Types

open Global.Types

type GitLog = {
    Message : string
    Commit : string
    Date : string
}

type GitBranch = {
    Name : string
    Log : GitLog []
}

type Msg =
    | Batch of Msg[]
    | Popup_Msg of Popup.Types.PopupStyle


type Model = {
    Info : string
}


module Main.State

open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open Browser.Dom
open Types
open JsInterop
open Global.Types

let init result =
    {
        Info = ""
    }


let update msg (model:Model) : Model * GlobalMsg * Cmd<Msg> =
    match msg with
    | Types.Batch msgs ->
        model,GlobalMsg.MsgNone, []
    | Popup_Msg style ->
        model,GlobalMsg.MsgNone, []




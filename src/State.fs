module App.State

open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open Browser.Dom
open Types
open Global.Types

let pageParser: Parser<Page->Page,Page> =
    oneOf [
      map (HomePage(RcoUpdate)) (s "home")
      map VerificationPage (s "verify")
      map (HomePage(RcoUpdate)) top 
    ]

let init result =
    {
        Main = Main.State.init ()
        Popup = Popup.Types.PopupStyle.Popup_Is_Dead
    }, Cmd.none


let update msg (model:Model) : Types.Model * Cmd<Types.Msg> =
    match msg with
    | MainMsg mainMsg ->
        let (mainModel,globalMsg,main_msg_cmd) = Main.State.update mainMsg model.Main

        let global_msg_cmd =
            globalMsg |>
            (
                Global >>
                Cmd.ofMsg
            )

        let app_cmd = Cmd.map MainMsg main_msg_cmd

        let msgsCombined =
            [|
                global_msg_cmd
                app_cmd
            |]

        { model with Main = mainModel}, Cmd.batch msgsCombined
        
    | Global globalMsg ->
        match globalMsg with
        | Batch msgs ->
            let msgsIntoGlobal =
                msgs
                |> Array.map (fun globalMsg ->
                    globalMsg |>
                    (
                        Global >>
                        Cmd.ofMsg
                    ))
            model, Cmd.batch msgsIntoGlobal
        | GlobalMsg.MsgNone ->
            model, []
        
        | Popup_Msg_Global style ->
            { model with Popup = style }, []
                
        


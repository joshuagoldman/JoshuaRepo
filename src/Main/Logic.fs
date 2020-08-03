module Main.Logic

open JsInterop
open Elmish
open Fable.Import
open System
open Types
open Global.Types
open Feliz

let standardPositions = {
        Popup.Types.PosX = 250.0
        Popup.Types.PosY = 250.0
    }

let QuestionPopup msg yesMsg noMsg dispatch positions =

    let questionToAsk =
        msg
        |> Popup.View.getPopupMsg

    let yesNoButtons =
        Popup.View.yesNoButtons yesMsg noMsg dispatch
        |> List.toArray

    let popupStyle =
        (yesNoButtons,questionToAsk)
        |> Popup.Types.Alternative_Popup_Otpions.Several_Alternatives

    (popupStyle,positions) |>
    (
        Popup.Types.PopupStyle.Has_Alternatives >>
        Global.Types.Popup_Msg_Global >>
        delayedMessage 3000
    )

let errorPopupMsg dispatch msgToDispatch positions msg =
    let reactElementMsgs =
        msg
        |> Popup.View.getPopupMsg

    let simpleOkButton =
        Popup.View.simpleOkButton msgToDispatch dispatch

    let allComponentsSimleOk =
        (simpleOkButton,reactElementMsgs)
        |> Popup.Types.Alternative_Popup_Otpions.Simple_Ok

    (allComponentsSimleOk,positions) |>
    (
        Popup.Types.PopupStyle.Has_Alternatives >>
        Main.Types.Popup_Msg
    )

let checkingProcessPopupMsg positions msg =

    (msg,positions) |>
    (
        Popup.Types.PopupStyle.Has_No_Alternatives >>
        Main.Types.Popup_Msg 
    )

let killPopupMsg =
    Popup.Types.PopupStyle.Popup_Is_Dead |>
    (
        Main.Types.Popup_Msg
    )

type GitUserDataType<'a> = {
    UserValue : 'a
    TypeName : string
}

type StringOrStringArray =
    | IsString of GitUserDataType<string option> 
    | IsStringArray of GitUserDataType<string[] option>  

let checkIfProcessExists dispatch = async{

    let prms = "shellCommand=which git"

    let! req = request prms

    match req.status with
    | 200.0 ->
        let isInstalled =
            "which: no"
            |> req.responseText.Contains
            |> not

        match isInstalled with
        | true ->
            ()
        | false ->
            ()

    | _ ->

        ()
}

    

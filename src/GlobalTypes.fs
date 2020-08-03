module Global.Types

open Fable.React
open Fable.Core.JsInterop
open Fable.Import

let getPositions ev =
    {
        Popup.Types.PosX = ( ev?pageX : float )
        Popup.Types.PosY = ( ev?pageY : float )
    }

let delayedMessage time ( msg : 'msg ) =
    async{
        if time < 30000 && time > 0
        then do! Async.Sleep time
        else do! Async.Sleep 0

        return(msg)
    }

let request ( data : obj ) = 
    Async.FromContinuations <| fun (resolve,_,_) ->
        let xhr = Browser.XMLHttpRequest.Create()
        xhr.``open``(method = "POST", url = "http://localhost:3001/shellcommand")
        xhr.setRequestHeader("Content-Type","application/x-www-form-urlencoded")
        

        xhr.onreadystatechange <- fun _ ->
            if xhr.readyState = (4 |> float)
            then
                resolve(xhr)

        xhr.send(data)

type PageOption =
    | RcoUpdate
    | Home

type Page =
    | VerificationPage
    | HomePage of PageOption

let tohashPageoptions page =
    match page with
    | RcoUpdate -> "#RcoUpdate"
    | Home -> "#Home"

let toHash page =
    match page with
    | VerificationPage -> "#verify"
    | HomePage option -> option |> tohashPageoptions

type CheckProcess<'a,'b> =
    | CheckProcessStarted of 'a
    | CheckProcessEnded of 'b

type Process<'a> =
    | ProcessInitiated
    | CheckProcessEnded of 'a

type ProcessNoResult<'a> =
    | ProcessInitiated of 'a
    | ProcessEnded 

type GlobalMsg =
    | MsgNone
    | Popup_Msg_Global of Popup.Types.PopupStyle
    | Batch of array<GlobalMsg>



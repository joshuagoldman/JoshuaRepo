module App.Types

open Main.Types

type Msg =
    | Global of Global.Types.GlobalMsg
    | MainMsg of  Main.Types.Msg

type Model = {
    Main : Main.Types.Model
    Popup : Popup.Types.PopupStyle
}

module App.View

open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open Fable.Core.JsInterop
open Types
open App.State
open Feliz
open Feliz.style
open Fable.React

importAll "../sass/main.sass"

let root (model: Model) dispatch =
    Html.div[
    ]

open Elmish.React
open Elmish.Debug
open Elmish.HMR

// App
Program.mkProgram init update root
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withReactBatched "elmish-app"
|> Program.run

module Home.View

open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Browser
open Feliz

open Fable.React
open Fable.React.Props


let root model dispatch =
    Html.div[
    ]

  

open Elmish.React
open Elmish.Debug
open Elmish.HMR
open Elmish

// App
Program.mkProgram State.init State.update root
|> Program.toNavigable (parseHash State.pageParser) State.urlUpdate
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withReactSynchronous "elmish-app"
|> Program.run
module Home.State

open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open Browser
open Global
open Types

let pageParser: Parser<Page->Page,Page> =
    oneOf [
        map Home (s "about")
    ]

let urlUpdate (result : Page option) model =
    match result with
    | None ->
        console.error("Error parsing url")
        model, Navigation.modifyUrl (toHash model.CurrentPage)
    | Some page ->
        { model with CurrentPage = page }, []

let init result =
    let (model, cmd) =
        urlUpdate result
          { CurrentPage = Home }

    model, Cmd.batch [ cmd ]

let update msg model =
    match msg with
    | HomeMsg -> model, []

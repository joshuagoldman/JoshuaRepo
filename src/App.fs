module App

open Elmish
open Elmish.React
open Feliz
open System.Reflection
open System.Xml.Linq
open System.Xml.XPath

let executingAssembly = Assembly.GetExecutingAssembly()
let embStream = executingAssembly.GetManifestResourceStream("App.PicturesEmbeddedResource.xml")
let mutable XDoc = XDocument.Load(embStream)

type State =
    { Name : string
      Picture : string
      InfoIsVisible : bool}

type Animal =
    | Cat of State
    | Dog of State
    | Fish of State

let animalSeq = 
  seq[
    {    Name = "Cat" ; 
         Picture = XDoc.XPathSelectElement(".//File[@Name = 'Cat']").
                        XPathSelectElement(".//String").FirstAttribute.Value ;
         InfoIsVisible = false   }                    
    {    Name = "Dog" ; 
         Picture = XDoc.XPathSelectElement(".//File[@Name = 'Dog']").
                        XPathSelectElement(".//String").FirstAttribute.Value ;
         InfoIsVisible = false}

    {    Name = "Fish" ; 
         Picture = XDoc.XPathSelectElement(".//File[@Name = 'Fish']").
                        XPathSelectElement(".//String").FirstAttribute.Value ;
         InfoIsVisible = false}                                      
  ]  

let getAnimalByStr (str : string) =
    animalSeq
    |> Seq.exists(fun animal -> animal.Name = str)
    |> function 
        | _ when true ->

          animalSeq |> Seq.find(fun animal -> animal.Name = str)

        | _ -> animalSeq |> Seq.toArray |> fun x -> x.[0]

type Msg =
  | NewTextSearch of string  
  | PictureIsClicked of State

let init() =
    animalSeq |> Seq.toArray |> fun x -> x.[0]

let update (msg: Msg) (state: State): State =
    match msg with
    | NewTextSearch(str) -> 
      getAnimalByStr str
      
    | PictureIsClicked(state) ->
      {Name = state.Name ;
       Picture = state.Picture ;
       InfoIsVisible = if state.InfoIsVisible = true then false else true}

let title = 
  Html.p [
      prop.className "title" 
      prop.text "AnimalStuff" 
  ]

let picture (name : string) (dispatch : Msg -> unit)= 
  Html.img [
      prop.src name
      prop.
  ]

let textInput (name : string) (dispatch : Msg -> unit) =

  Html.div[
      prop.classes ["field"; "has-addons"]
      prop.children [
          Html.div [
              prop.classes [ "control" ; "is-expanded" ]
              prop.children[
                  Html.input[
                      prop.classes[ "input"; "is-medium"] 
                      prop.valueOrDefault name 
                      prop.onTextChange(fun text -> dispatch(NewTextSearch(text)))
                  ]
              ]
          ]

          Html.div[
              prop.classes [ "control" ]
              prop.children [
                  Html.button [
                    prop.classes [ "button" ; "is-primary" ; "is-medium" ]
                    prop.children [
                      Html.i [ prop.classes [ "fa" ; "fa-plus"] ]
                    ]   
                  ]
              ]
          ]
      ]
  ]

let render (state: State) (dispatch: Msg -> unit) =
  Html.div[
      prop.style [style.padding 20]
      prop.children [
        title
        textInput state.Name dispatch
      ]
  ]

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run
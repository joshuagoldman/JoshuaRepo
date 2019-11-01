namespace Vradim


module App =

  open Fable.React
  open Fable.React.Props
  open System
  open Elmish
  open Elmish.React
  open Fulma
  open System.IO
  open System.Reflection
  open System.Xml.Linq
  open System.Xml.XPath
  open System.Text
  open System.Drawing
  open Definitions


  type Animal =
      | Cat of State
      | Dog of State
      | Fish of State

  let getAnimalByStr (str : string) =
    let chosenState =
        animalSeq
        |> Seq.exists (fun animal -> animal.Name = str)
        |> function 
            | res when res = true -> 
                animalSeq |> Seq.find (fun animal -> animal.Name = str)
            | _ -> animalSeq |> Seq.item 0
          
    createNewTempPicture chosenState
    chosenState

  type Msg =
    | NewTextSearch of string * string  
    | InputTextHasChanged of string
    | PictureIsClicked of bool

  let init() : State =
      let chosenState =
        animalSeq |> Seq.toArray |> fun x -> x.[0]

      createNewTempPicture chosenState
      chosenState    

  let update (msg: Msg) (state: State): State =
      match msg with
      | NewTextSearch(name, pic) -> 
        {state with Name = name ; Picture = pic }

      | InputTextHasChanged(name) -> 
        {state with Name = name}
        
      | PictureIsClicked(isVisible) ->
        createNewTempPicture state
        {state with
            InfoIsVisible = if isVisible = true then false else true}

  let picture (state : State) (dispatch : Msg -> unit) =
    img 
     [ 
        Src ("C:\Users\DELL\Pictures\\StateImage.jpg")
        Alt(state.Name)
        OnClick (fun _ -> dispatch(PictureIsClicked(state.InfoIsVisible)))
     ]

  let title = 
    label [Class "label"]
     [
        str "Info"
     ]

  let textInput (name : string) (dispatch : Msg -> unit) =
    div [classList ["field",true ; "has-addons", true] ]
     [
        div [classList ["control", true ; "is-expanded", true]]
         [
            input 
             [
                classList ["input", true ; "is-medium", true]
                OnKeyUp (fun ev -> InputTextHasChanged(ev.Value) |> dispatch )
             ]
         ]

        div [Class "field is-grouped"]
         [
            button
             [
                classList ["control",true ; "button is-link", true]
                OnClick (fun _ -> NewTextSearch(getAnimalByStr name 
                                                |> fun x -> x.Name, x.Picture) 
                                                |> dispatch)
             ]
             [str "Search"]
         ]
     ]
    
  let decideIfVisible (state : State) =
    match state.InfoIsVisible with
     | true -> "This is info about " + state.Name
     | _ -> state.Name

  let texArea (state : State) =
    div [Class "field"]
     [
        div [Class "control"]
         [
            text [Class "textarea" ; Alt(state.Name)] 
             [
                str (decideIfVisible state)
             ]
            
         ]
     ]



  let render (state: State) (dispatch: Msg -> unit) =
    div [] 
     [
       title
       textInput state.Name dispatch
       picture state dispatch
       texArea state
     ]

  open Elmish.Debug

  Program.mkSimple init update render
  |> Program.withConsoleTrace
  |> Program.withReactSynchronous "elmish-app"
  |> Program.run
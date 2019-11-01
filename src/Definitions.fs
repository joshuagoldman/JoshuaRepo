module Definitions

open System
open Fulma
open System.IO
open System.Reflection
open System.Xml.Linq
open System.Xml.XPath
open System.Drawing


let executingAssembly = Assembly.GetExecutingAssembly()
let embStream = executingAssembly.GetManifestResourceStream("App.PicturesEmbeddedResource.xml")
let mutable XDoc = XDocument.Load(embStream)


type State =
    { Name : string
      Picture : string
      InfoIsVisible : bool}

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


let createNewTempPicture (state : State) =
    let byteArr = Convert.FromBase64String state.Picture
    using(new MemoryStream(byteArr, 0, byteArr.Length))
        (fun stream ->
            if File.Exists("C:\Users\DELL\Pictures\\StateImage.jpg")
            then File.Delete("C:\Users\DELL\Pictures\\StateImage.jpg")
            let img = Image.FromStream stream
            img.Save("C:\Users\DELL\Pictures\\StateImage.jpg")
        )

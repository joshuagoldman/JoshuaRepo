namespace Vradim.Tests

open System
open Xunit
open Vradim.App
open System.Xml.Linq
open System.Xml.XPath
open System.Reflection
open System.IO
open Definitions
open System.Reflection

module Tests =

    let executingAssembly = Assembly.GetAssembly(typeof<Vradim.App.Animal>)
    let embStream = executingAssembly.GetManifestResourceStream("App.PicturesEmbeddedResource.xml")
    let mutable XDoc = XDocument.Load(embStream) 
    
    let anSeq = animalSeq

    let initAnimal =
        {    Name = "Fish" ; 
             Picture = XDoc.XPathSelectElement(".//File[@Name = 'Fish']").
                            XPathSelectElement(".//String").FirstAttribute.Value ;
             InfoIsVisible = false   
             }  

    [<Fact>]
    let ``testFileCreation`` () =
        createNewTempPicture initAnimal

    [<Fact>]
    let ``RightAnimalIsChosen`` () =
        
        let chosenAnimal = 
            {   Name = "Cat" ; 
                Picture = XDoc.XPathSelectElement(".//File[@Name = 'Cat']").
                           XPathSelectElement(".//String").FirstAttribute.Value ;
                InfoIsVisible = false   
            } 
        
        let msg = NewTextSearch("Fish", "")

        let dir = Directory.GetCurrentDirectory()

        let animalNew = update msg chosenAnimal

        Assert.True(animalNew.Name = "Fish")


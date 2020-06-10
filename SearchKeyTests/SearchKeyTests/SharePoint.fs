module SharePoint

open System
open Xunit
open Ericsson.AM.Common.Definitions
open Ericsson.AM.Common.Logging
open Ericsson.AM.LogAnalyzer.BLL
open Ericsson.AM.LogAnalyzer.Definitions
open Ericsson.AM.LogAnalyzer.Models
open System
open System.Collections.Generic
open System.Data
open System.IO
open System.Linq
open System.Xml
open Ericsson.AM.LogAnalyzer
open Xunit.Abstractions
open System.Text.RegularExpressions

type LogExistance<'t> =
| Exist of 't
| NotExist

type LogSearchResult =
| Hit of string
| NoHit

type CountryIDPair = 
    {
        Name : string
        ID : string
    }

[<Fact>]
[<Trait("Category","SharePoint Actionst")>]
let ``CreateFolders`` () =
    let siteInfo4ScriptPath = "C:\Users\egoljos\Documents\ScriptDocuments\SiteInfoForScript.xml"

    let siteInfoContent = File.ReadAllText siteInfo4ScriptPath 

    let sitesRegex = new Regex("Site (?! )(.|\n)*?\/Site")
    let siteRegex = new Regex("(?<=Site Name=\").*(?=\">)")
    let IDRegex = new Regex("(?<=<ServiceLocationIVI>).*(?=<)")

    let mainPath = "c:/Temp"

    let siteChunks =
        let chunkMatches = sitesRegex.Matches(siteInfoContent)

        [0..chunkMatches.Count - 1]
        |> Seq.map (fun pos ->
            chunkMatches.[pos].Value)

    let allSiteInfos =
        siteChunks
        |> Seq.map (fun chunk ->
            let site = siteRegex.Match(chunk).Value
            
            let id = IDRegex.Match(chunk).Value
            
            {
                Name = site
                ID = id
            })

    let createFolder ( country : CountryIDPair ) =
        let newFolder = mainPath + "/" + country.ID + "_R10M_1"

        let newDir = Directory.CreateDirectory(newFolder)

        newDir.Create()

        let files =
            seq[
                "LAT_R10M_1_Rel-" + country.Name + ".zip"
                "PRTT_R10M_1_Rel-" + country.Name + ".zip"
            ]

        files
        |> Seq.iter(fun file -> 
            ()
            |> function
                | _ when File.Exists(mainPath + "/" + file) ->
                    ()
                    |> function
                        | _ when file.Contains("LAT") ->
                            let newLATDir = Directory.CreateDirectory(newFolder + "/" + "LAT/R10M_1")
                            
                            newLATDir.Create()

                            File.Copy( mainPath + "/" + file, newLATDir.FullName + "/" + file)
                        | _  -> 
                            let newPRTTTDir = Directory.CreateDirectory(newFolder + "/" + "PRTT/R10M_1")
                            
                            newPRTTTDir.Create()

                            File.Copy(mainPath + "/" + file, newPRTTTDir.FullName + "/" + file)
                    
                | _ -> ())

    
    allSiteInfos
    |> Seq.filter (fun site ->
        site.Name <> "Dev-Team")
    |> Seq.iter (fun site -> site |> createFolder)
    
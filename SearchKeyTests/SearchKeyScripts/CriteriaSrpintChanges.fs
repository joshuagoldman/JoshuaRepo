module CriteriaSrpintChanges

open System.IO
open System.Xml.Linq
open System.Xml.XPath
open System
open System.Text.RegularExpressions
open System.Linq
open System.Diagnostics
open GitSharp

type CriteiaVersions = {
    PrevVersion : XDocument
    LatestVersion : XDocument
}

type CriteriaWRevision = {
    Criteria : string
    Revision : string
}

type AdditionalInfo = {

    CriteriaName : string
    TextOutput : string
    FullDocNumber : string
}

type CriteriaCompleteInfo = {
    BaseInfo : CriteriaWRevision
    CriteriaName : string
    InfoText : string
    FullDocNumber : string
}

type CriteriaInfos = {
    OldCriteira : seq<CriteriaCompleteInfo>
    UpToDateCriteria : seq<CriteriaCompleteInfo>
}

let executeCommand command =
    let gitProcess = new System.Diagnostics.Process()
    let gitInfo = new System.Diagnostics.ProcessStartInfo()
    gitInfo.Arguments <- command // such as "fetch origin"
    gitInfo.UseShellExecute <- false; 
    gitInfo.WorkingDirectory <- "C:/Users/egoljos/Documents/Gitrepos/LogAnalyzer"
    gitInfo.FileName <- "git"
    gitInfo.RedirectStandardOutput <- true
    gitProcess.StartInfo <- gitInfo
    gitProcess.Start()
    |> function
    | processStartedSuccesfully when processStartedSuccesfully = true ->
            //gitProcess.WaitForExit()
            let allStringinfo = 
                gitProcess.StandardOutput.ReadToEnd()
                
            gitProcess.Close()


            allStringinfo
            |> Some
    | _ -> None

let getTwoDifferentFileVersionsByCommit (command1, command2) =

    let first = executeCommand command1
    let second = executeCommand command2
    let result =
        {
            PrevVersion = XDocument.Parse(first.Value)
            LatestVersion = XDocument.Parse(second.Value)
        }
    result


let cmd1 =
    "show HEAD@{2020-05-21}:Ericsson.AM.LogAnalyzer/EmbeddedCriteria/RBS6000/Aftermarket/HWLogCriteria.xml"
    
let cmd2 =
    "show HEAD@{2020-06-04}:Ericsson.AM.LogAnalyzer/EmbeddedCriteria/RBS6000/Aftermarket/HWLogCriteria.xml"

let twoFiles =
    let result =
        (cmd1,cmd2)
        |> getTwoDifferentFileVersionsByCommit

    result

let searchKeyNameAndTxtOutput ( xDoc : XDocument ) =
    let searchKeyElements =
        xDoc.XPathSelectElements(".//SearchKey")

    let nn = searchKeyElements |> Seq.take 10
        
    searchKeyElements
    |> Seq.filter (fun el ->
        el.ToString().Contains("Infotext Value"))
    |> Seq.map (fun el ->
        let searchKeyName =
            el.FirstAttribute.Value
        let txtOuput =
            el.XPathSelectElement(".//Infotext").FirstAttribute.Value.Replace("\n","<br>")

        let fullDocNumber =
                el.XPathSelectElement(".//CriteriaReferenceWithRevision").FirstAttribute.Value
                |> fun x ->
                    x.Replace(",", ", Rev ").Replace(";", ", Rev ")
        {
            CriteriaName = searchKeyName
            TextOutput = txtOuput
            FullDocNumber = fullDocNumber
        })

let findAdditionalInfo ( allInfo : seq<AdditionalInfo> )
                       ( searchWord : string ) =

    allInfo
    |> Seq.tryFind (fun info ->
        info.CriteriaName.Replace(" ","").Contains(searchWord.Replace(" ","")))

let getAllCriteriasAndRevisions ( xDoc : XDocument ) =
    let xPath =
        "//CriteriaReferenceWithRevision"
    let elements =
        xDoc.XPathSelectElements(xPath)

    let len = elements |> Seq.length

    let sequenceOfCriteiraInformation =
        elements
        |> Seq.map (fun el ->
            let strToWorkWith =
                el.FirstAttribute.Value.Replace(" ","")
            
            let searchKey = 
                let firstPart = strToWorkWith.Replace("15451-LPA108338","").Replace("Uen","")

                ()
                |> function
                    | _ when firstPart.Contains(";") ->
                        let finalPart = firstPart.Substring(0,firstPart.LastIndexOf(";"))
                        finalPart
                    | _ when firstPart.Contains(",") ->
                        let finalPart = firstPart.Substring(0,firstPart.LastIndexOf(","))
                        finalPart
                    | _ -> "Couldn't parse criteria"
                

            let revision = Regex.Match(strToWorkWith, "(?<=;).*|(?<=,).*").Value.Replace(" ","")

            let result =
                {
                    Criteria = searchKey
                    Revision  = revision
                }
            result)
        |> Seq.distinctBy (fun info ->
            info.Criteria)
    let allInfo =
        xDoc |> searchKeyNameAndTxtOutput
    let test =
        sequenceOfCriteiraInformation
        |> Seq.item 0

    sequenceOfCriteiraInformation
    |> Seq.map (fun info ->
        info.Criteria
        |> findAdditionalInfo allInfo
        |> function
            | res when res.IsSome ->
                {
                    BaseInfo = info
                    CriteriaName = res.Value.CriteriaName
                    InfoText = res.Value.TextOutput
                    FullDocNumber = res.Value.FullDocNumber
                }
            | _ -> 
                {
                    BaseInfo = info
                    CriteriaName = info.Criteria + ", rev " + info.Revision
                    InfoText = ""
                    FullDocNumber = ""
                })


let critInfos =
    {
        OldCriteira = twoFiles.PrevVersion |> getAllCriteriasAndRevisions
        UpToDateCriteria = twoFiles.LatestVersion |> getAllCriteriasAndRevisions
    }
    
let getSprintCriteriaUpdate (revisions : CriteriaInfos) =
    let oldCriteriasAsString =
        revisions.OldCriteira
        |> Seq.map (fun crit ->
            crit.BaseInfo.Criteria + "HASREV" + crit.BaseInfo.Revision)
        |> String.concat ""

    let newCriteriasAsString =
        revisions.UpToDateCriteria
        |> Seq.map (fun crit ->
            crit.BaseInfo.Criteria + "HASREV" + crit.BaseInfo.Revision)
        |> String.concat ""
    
    let newCriterias =
        revisions.UpToDateCriteria
        |> Seq.choose (fun upToDateCrit ->
            let critExistsPattern = 
                new Regex(upToDateCrit.BaseInfo.Criteria + "H")
            let critExists =
                critExistsPattern.Match(oldCriteriasAsString).Success
            ()
            |> function
                | _ when critExists = true ->
                    None
                | _ -> Some upToDateCrit)
        |> function
            | res when res |> Seq.length <> 0 ->
                Some res
            | _ -> None

    let updateCriterias =
        revisions.UpToDateCriteria
        |> Seq.choose (fun upToDateCrit ->
            let critExistsPattern = 
                new Regex(upToDateCrit.BaseInfo.Criteria + "H")
            let critExists =
                critExistsPattern.Match(oldCriteriasAsString).Success

            let critIsUpdatePattern = 
                new Regex(upToDateCrit.BaseInfo.Criteria + "HASREV" + upToDateCrit.BaseInfo.Revision)
            let critIsNotUpdate =
                critIsUpdatePattern.Match(oldCriteriasAsString).Success

            ()
            |> function
                | _ when critExists = true && critIsNotUpdate = false ->
                    Some upToDateCrit
                | _ -> None)
        |> function
            | res when res |> Seq.length <> 0 ->
                Some res
            | _ -> None

    let newCriteriasString =
        newCriterias
        |> function
            | criterias when criterias.IsSome ->
                newCriterias.Value
                |> Seq.map (fun crit ->
                    String.Format(
                        "New LAT Criteria: {0}£{1}£{2}
",
                        crit.CriteriaName,
                        crit.FullDocNumber,
                        crit.InfoText
                    ))
                |> String.concat "$"
            | _ -> "" 

    let updateCriteriasString =
        updateCriterias
        |> function
            | criterias when criterias.IsSome ->
                updateCriterias.Value
                |> Seq.map (fun crit ->
                    String.Format(
                        "Update: {0}£{1}£{2}
",
                        crit.CriteriaName,
                        crit.FullDocNumber,
                        crit.InfoText
                    ))
                |> String.concat "$"
            | _ -> "" 


    newCriteriasString + "\n\n\n" + updateCriteriasString

let finalString =
    critInfos
    |> getSprintCriteriaUpdate

File.WriteAllText("C:\Users\egoljos\Documents\ScriptDocuments\CriteriasForSprint.txt",finalString)
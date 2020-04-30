module CheckExRstatePQATCriterias

open System.Text.RegularExpressions
open System
open System.IO


let hwlogCritContent = 
    File.ReadAllText("C:\Users\egoljos\Documents\Gitrepos\LogAnalyzer\Ericsson.AM.LogAnalyzer\EmbeddedCriteria\RBS6000\Aftermarket\HWLogCriteria.xml")

let customFunctionsClassContent =
    File.ReadAllText("C:\Users\egoljos\Documents\Gitrepos\LogAnalyzer\Ericsson.AM.LogAnalyzer\CustomFunctions.cs")

let methodsRegex = 
    new Regex("((?<=private |public ))(\n|.)*?(return)(?![a-zA-Z])")

let methodsMatches = 
    methodsRegex.Matches(customFunctionsClassContent)

let methodStrings =
    let len = methodsMatches.Count

    let result =

        [|0..len-1|]
        |> Array.map (fun pos -> methodsMatches.Item(pos))
        |> Array.map (fun regexMatch -> regexMatch.Value)
        |> Array.toSeq
    
    result
    
let pqatCallingMethodsRegex =
    new Regex("(?<=>>\s).*(?=\()")

let iPqatClassContent = 
    File.ReadAllText("C:\Users\egoljos\Documents\Gitrepos\LogAnalyzer\Ericsson.AM.Pqat\IPQATClient.cs")

let pqatMethodsRegexString = 
    let pqatCallingMethodsMatches = pqatCallingMethodsRegex.Matches(iPqatClassContent)
    let len = pqatCallingMethodsMatches.Count

    let result =
    
        [|0..len-1|]
        |> Array.map (fun pos -> pqatCallingMethodsMatches.Item(pos))
        |> Array.map (fun regexMatch -> regexMatch.Value)
        |> Array.toSeq
        |> Seq.map (fun str -> str + "|")
        |> String.concat ""
        |> fun x -> x.Substring(0, x.LastIndexOf("|"))
        |> fun x -> "(" + x + ")"
        |> fun x -> x.Replace("<","\<").Replace(">","\>")

    result  

let pqatMethodWithPqatStringRegex =
    let pqatMethodWithPqatRegex = new Regex(pqatMethodsRegexString, RegexOptions.ECMAScript)
    let methodNamesRegex = new Regex("(?<=(int|bool|string|void|List\<.*\>)\s).*(?=\()", RegexOptions.ECMAScript)

    let result =

        methodStrings
        |> Seq.filter (fun cntnt -> pqatMethodWithPqatRegex.IsMatch(cntnt) )
        |> Seq.filter (fun cntnc -> methodNamesRegex.IsMatch(cntnc))
        |> function
            | res when res |> Seq.length > 0 ->
                res
                |> Seq.map (fun str ->  methodNamesRegex.Match(str).Value)
                |> Seq.map (fun str -> str + "|")
                |> String.concat ""
                |> fun x -> x.Substring(0, x.LastIndexOf("|"))
                |> fun x -> "(" + x + ")"

            | _ -> ""

    result

let HwLogCriteriaAllSEarchKeysRegex = 
    let result =
        new Regex("(\<SearchKey Name)(.|\n)*?(\<\/SearchKey>)", RegexOptions.ECMAScript)

    result

let HwLogCriteriaSearchKeysNoExcludedServiceLocations = 
    let result =
        new Regex( pqatMethodWithPqatStringRegex + "(?!(.|\n)*(\<ExcludeServiceLocations))", RegexOptions.ECMAScript)

    result

let searchKeyNameRegex =
    new Regex("(?<=\<SearchKey Name=\").*(?=\"\>)", RegexOptions.ECMAScript)

let searchKeysNoExcludedServiceLoc = 
    let matches = HwLogCriteriaAllSEarchKeysRegex.Matches(hwlogCritContent)
    let len = matches.Count

    let result =

        [|0..len-1|]
        |> Array.map (fun pos -> matches.Item(pos))
        |> Array.map (fun regexMatch -> regexMatch.Value)
        |> Array.toSeq
        |> Seq.filter(fun matchVal -> HwLogCriteriaSearchKeysNoExcludedServiceLocations.IsMatch(matchVal))
        |> function
            | res when res |> Seq.length <> 0 ->
                res
                |> Seq.map(fun searchKey -> searchKeyNameRegex.Match(searchKey).Value + "\n")
                |> String.concat ""
                |> fun x -> x.Substring(0,x.LastIndexOf("\n"))
            | _ -> ""
        |> fun x -> x.Substring(0, x.LastIndexOf("\n"))

    result

File.WriteAllText("C:\Users\egoljos\Documents\Ericsson\SearchKeys_No_ExcludeServiceLoc.txt",searchKeysNoExcludedServiceLoc)

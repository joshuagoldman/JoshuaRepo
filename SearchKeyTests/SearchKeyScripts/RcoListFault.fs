module RcoListFault

open System
open System
open System.Collections.Generic
open System.Data
open System.IO
open System.Linq
open System.Xml
open System.Text.RegularExpressions


let fileContent =
    File.ReadAllText("C:\Users\egoljos\Documents\Gitrepos\LogAnalyzer\Ericsson.AM.RcoHandler\EmbeddedResources\RBS6000\Aftermarket\RBS RCO List.csv")

let resultString =
    let result = 
        fileContent
        |> fun str -> str.Split([|"\r\n"|] , StringSplitOptions.None) 
                      |> Seq.map (fun row -> row.Split([|"&@?"|] , StringSplitOptions.None) 
                                             |> Array.toSeq)
        |> fun allInfo -> allInfo
                          |> Seq.item 0
                          |> fun firstRow -> 
                             Seq.zip firstRow [0..firstRow |> Seq.length |> fun x -> x - 1]
                             |> Seq.tryFind (fun (col,pos) -> col = "R-stateIN")
                             |> function
                                 | res when res <> None ->
                                         res.Value
                                         |> fun (_,pos) ->
                                                 Seq.zip allInfo [0..allInfo |> Seq.length |> fun x -> x - 2]
                                                 |> Seq.map (fun (col,rowPos) -> col
                                                                                 |> Seq.item pos
                                                                                 |> fun rStateInCol -> 
                                                                                     rStateInCol + " " + (rowPos + 1|> string) + "\n")
                                                 |> String.concat ""
                                 | _ -> ""

    result

let matches = 
        let faultsRegex = Regex("^(?!(All\s[0-9])|((-|\+))?R([0-9]|10)([A-Z]|\s)(?:\/[A-Z])?).*", RegexOptions.Multiline )
        let matchCount =
            let counInitial = faultsRegex.Matches(resultString).Count

            let matches =
                seq[0..counInitial - 1]
                |> Seq.map (fun pos -> faultsRegex.Matches(resultString).[pos])
                |> Seq.filter (fun matchRegex -> matchRegex.Value <> "R-stateIN 1")

            matches
            |> Seq.length

        let matches = faultsRegex.Matches(resultString)
        let rowRegex = Regex("\s[0-9].*")

        let faultRows = 
            seq[0..matchCount - 1]
            |> Seq.map (fun pos -> 
                let rowNumberMatchVal = 
                    rowRegex.Match(matches.Item(pos).Value).Value
                "Row: " + rowNumberMatchVal + ", Value: \"" + matches.Item(pos).Value.Replace(rowNumberMatchVal,"") +  "\"\n")
            |> String.concat ""

        File.WriteAllText("C:\Users\egoljos\Documents\ScriptDocuments\RcoListCheck.txt", resultString + "\n" + faultRows)

        matchCount






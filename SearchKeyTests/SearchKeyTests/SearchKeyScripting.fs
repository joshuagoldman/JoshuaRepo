module SearchKeyScripting

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

[<Fact>]
[<Trait("Category","Search Key Scripting")>]
let ``SearchKey_2_53`` () =
    let infoString =
        "KRC 161297/2	R1D
        KRC 161297/2	R1E
        KRC 161297/2	R1F
        KRC 161 262/2	R1B
        KRC 161 262/2	R1C
        KRC 161 262/2	R1D
        KRC 161 262/2	R1F
        KRC 161 262/2	R1G
        KRC 161 282/2	R1B
        KRC 161 282/2	R1E/A
        KRC 161 282/2	R1E
        KRC 161 282/2	R1F/A
        KRC 161 282/2	R1F
        KRC 161 282/2	R1H/A
        KRC 161 282/2	R1H
        KRC 161 325/2	R1B
        KRC 161 325/2	R1D
        "

    let getMatches ( regex : Regex ) input =
        let matchs = regex.Matches(input)

        [0..matchs.Count - 1]
        |> Seq.map (fun pos ->
            matchs.[pos].Value.Trim())

    let chunkRegex =
        new Regex(".*\w")

    let productsRegex =
        Regex("KRC.*(?=R)")

    let rstateRegex =
        Regex("(?<!K)R.*")

    let chunks =
        infoString
        |> getMatches chunkRegex

    let prodInfos =
        chunks
        |> Seq.map (fun chunk ->
            {|
                Product = productsRegex.Match(chunk).Value.Trim()
                Rstate = rstateRegex.Match(chunk).Value.Trim()
            |})

    let prodVariety = 
        prodInfos
        |> Seq.distinctBy (fun info -> info.Product)
        |> Seq.map (fun info -> info.Product)

    let prodsInfoDistinct =
        prodVariety
        |> Seq.map (fun prodNumber ->
            let rstates =
                prodInfos
                |> Seq.filter (fun info ->
                    info.Product = prodNumber)
                |> Seq.map ( fun info ->
                    info.Rstate)
                |> String.concat ","
            
            {|
                Product = prodNumber
                Rstate = rstates
            |}
            )

    let finalString =
        prodsInfoDistinct
        |> Seq.map (fun info ->
            String.Format(
                "<Product ProductNumber=\"{0}\" RState=\"{1}\" />",
                info.Product,
                info.Rstate
            ))
        |> String.concat "\n"

    File.WriteAllText("C:\Users\egoljos\Documents\ScriptDocuments\SK_2_53.txt",finalString)


type BoardInfo = {
    ProdNumber : string
    Board : string
    Branch : string
}

[<Fact>]
[<Trait("Category","Search Key Scripting")>]
let ``SearchKey_2_48`` () =
    
    let table = 
        "KRC161707/1	Radio 8843 B2 B66A	A	ROA 128 6596/02	ROZ 104 2065/02	N100GAT	RYT 101 6796/1
        KRC161707/1	Radio 8843 B2 B66A	B 			N100HAT	RYT 101 6796/1
        KRC161707/1	Radio 8843 B2 B66A	C			N100PAT	RYT 101 6796/1
        KRC161707/1	Radio 8843 B2 B66A	D			N100QAT	RYT 101 6796/1
        KRC161707/1	Radio 8843 B2 B66A	E	ROA 128 6596/66A	ROZ 104 2065/66A	N100QAT	RYT 101 6796/1
        KRC161707/1	Radio 8843 B2 B66A	F			N100PAT	RYT 101 6796/1
        KRC161707/1	Radio 8843 B2 B66A	G			N100HAT	RYT 101 6796/1
        KRC161707/1	Radio 8843 B2 B66A	H			N100GAT	RYT 101 6796/1"

    
    let rowRegex = new Regex("KRC.*")

    let productsRegex = new Regex("KRC.*?(\/[0-9]{1,1})")

    let refDesRegex = new Regex("N.*?T")

    let partRegex = new Regex("RYT.*")

    let branchRegex = new Regex("(?<=	)[a-h A-H](?=.*(ROA|N))")

    let boardChunkRegex = new Regex("KRC.*ROA.*(?=ROZ)(.|\n)*(?=\n.*ROA)|KRC.*ROA.*(?=ROZ)(.|\n)*")

    let boardRegex = new Regex("ROA.*(?=ROZ)")

    let boardChunks =
        let rowMatches = boardChunkRegex.Matches(table)
        
        [0..rowMatches.Count - 1]
        |> Seq.map (fun pos ->
            rowMatches.[pos].Value)

    let dictRows rows board = 
        rows
        |> Seq.map (fun row ->
            let productNumber = productsRegex.Match(row).Value
            
            let refDes = refDesRegex.Match(row).Value
            
            let part = partRegex.Match(row).Value.Replace("\r","")
            
            let branch = branchRegex.Match(row).Value
            
            let dictRow =
                String.Format(
                    "new BranchIDComponentInfo
{{
    ProductNumber = \"{0}\",
    Branch = \"{1}\",
    REF_des = \"{2}\",
    Part = \"{3}\",
    Board = \"{4}\"
}}",
                    productNumber,
                    branch,
                    refDes,
                    part,
                    board)
            dictRow)

    let allInfo =
        boardChunks
        |> Seq.collect (fun chunk ->
            let rows =
                let rowMatches = rowRegex.Matches(chunk)
                
                [0..rowMatches.Count - 1]
                |> Seq.map (fun pos ->
                    rowMatches.[pos].Value)
            let board = boardRegex.Match(chunk).Value.Trim()

            let result = dictRows rows board
            
            result)
        |> String.concat ",
"
            

    File.WriteAllText("C:\Users\egoljos\Documents\ScriptDocuments\Criteria_2_48_dict.txt",allInfo)

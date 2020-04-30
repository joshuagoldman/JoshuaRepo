module WriteNewProducts2Add

open System.IO
open System.Xml.Linq
open System.Xml.XPath
open System
open System.Text.RegularExpressions
open System.Linq

type NewProdsInfo = {
    Name : string
    ProductNumber : string
    Bands : seq<string> option
    Power : string option
    FrequencyWidth :  string option
}

type PRTTTestCaseInfo = {
    ProductNumber : string
    Name : string
    TestType : string
    Revision : string
    CategoryNumber : string
    TestCaseID : string
    Description : string
}

let newProductsString =
    File.ReadAllText("C:\Users\egoljos\Documents\ScriptDocuments\NewProductsToAdd.txt")

let prodNameRegex =
    new Regex("(?<=LAT:|LAT:Addsupportfor)(R|I).*?(?=,)")
        
let prodNumberRegex =
    new Regex("(?<=LAT:.*,).*?(?=Compl)")

let getSequence ( text : string ) 
                ( regex : Regex ) =
    let matches =
        regex.Matches(text.Replace(" ","").Replace("\n",""))

    let len = matches.Count

    let result =
        [0..len - 1]
        |> Seq.map (fun pos -> matches.[pos].Value.Trim())

    result

let prodNumbers =
    prodNumberRegex
    |> getSequence newProductsString

let prodName =
    prodNameRegex
    |> getSequence newProductsString

let bands =
    let bandsRegex =
        new Regex("(?![0-500])B.*?(?=\s|\n|$|\/)")

    prodName
    |> Seq.map (fun name ->
        bandsRegex.IsMatch(name)
        |> function
            | res when res = true ->
                let bandsMatches =
                    bandsRegex.Matches(name)

                [0..bandsMatches.Count - 1]
                |> Seq.map (fun pos -> bandsMatches.[pos].Value) |> Some

            | _-> None)

let info =
    Seq.zip prodNumbers [0..prodNumbers |> Seq.length |> fun x -> x - 1]
    |> Seq.map (fun (prodNum,pos) -> 
        {
            ProductNumber = prodNum
            Name = prodName |> Seq.item pos
            Bands = bands |> Seq.item pos
            Power = None
            FrequencyWidth = None
        })


let templateInfoStringLAT =
    File.ReadAllText("C:\Users\egoljos\Documents\ScriptDocuments\TemplateInfo.xml")

let templateInfoStringPRTT =
    File.ReadAllText("C:\Users\egoljos\Documents\SteamTemplates\TemplateInfoPRTT.xml")

let allTestCasesWithInfo info =
    let allTestCasesRegex = 
        new Regex("(<TestCase(?!ID))(\n|.)*?(?=<\/TestCase(?!ID))")

    let allMatches =
        allTestCasesRegex.Matches(info)
    
    let finalResult =
        [0..allMatches.Count - 1]
        |> Seq.map (fun pos ->
            allMatches.[pos].Value)

    finalResult

let allProdNumbersForRevisionR10KLAT =
    let prodNumberRegex =
        new Regex("(?<=<ProductNo>).*?(?=<\/ProductNo>)")
    
    let finalResult =
        templateInfoStringLAT
        |> allTestCasesWithInfo
        |> Seq.map (fun tstCase ->
            prodNumberRegex.Match(tstCase).Value)

    finalResult

let getProperty ( propName : string ) input =
    let regexStr =
        String.Format(
            "(?<=<{0}>).*?(?=<\/{0}>)",
            propName
        )
    let regexToWorkWith =
        new Regex(regexStr)

    let result =
        regexToWorkWith.Match(input).Value

    result

let getAllProdInfo xmlFilePath revision =
    let xmlStr =
        File.ReadAllText(xmlFilePath)

    let finalResult =
        xmlStr
        |> allTestCasesWithInfo
        |> Seq.map (fun tstCase ->
            {
                ProductNumber = getProperty "ProductNo" tstCase
                Name = (getProperty "Name" tstCase).Replace("R10K",revision)
                TestType = (getProperty "TestType" tstCase).Replace("R10K",revision)
                Revision = (getProperty "R-State" tstCase).Replace("R10K",revision)
                CategoryNumber = (getProperty "CategoryNo" tstCase).Replace("R10K",revision)
                TestCaseID = (getProperty "TestCaseID" tstCase).Replace("R10K",revision)
                Description = (getProperty "Description" tstCase).Replace("R10K",revision)
            })

    finalResult
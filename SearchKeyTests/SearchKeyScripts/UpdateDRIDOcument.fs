module UpdateDRIDOcument

open System.IO
open System.Xml.Linq
open System.Xml.XPath
open System
open System.Text.RegularExpressions
open System.Linq


let HwLogCriteriaStream = File.Open("C:\Users\egoljos\Documents\Gitrepos\LogAnalyzer\Ericsson.AM.LogAnalyzer\EmbeddedCriteria\RBS6000\Aftermarket\HWLogCriteria.xml", FileMode.OpenOrCreate)
let mutable xDoc = XDocument.Load(HwLogCriteriaStream)

let products =
    let xPath =
        "*//Product [@ProductNumber]"

    let result =
        xDoc.XPathSelectElements(xPath)
        |> Seq.distinct
        |> Seq.map (fun el ->
            el.FirstAttribute.Value)

    result


type SearchKeyNumbers = {
    Ones : seq<string>
    Twos : seq<string>
}

let xdocAsString =
    xDoc.ToString()

let allSearchKeyNumbers =
    let regex4Ones =
        "(?<=1\/)-[0-9]{1,2}"

    let regex4Twos =
        "(?<=2\/)-[0-9]{1,2}"

    let matchesSequence pattern =
        let matchesAll =
            Regex.Matches(xdocAsString,pattern)

        [0..matchesAll |> Seq.length |> fun x -> x - 1]
        |> Seq.map (fun pos -> 
            matchesAll.[pos].Value)

    let getNumbers sequence =
        sequence 
        |> matchesSequence
        |> Seq.distinctBy (fun strValue ->
            strValue.Replace("-","")
            |> int)
        |> Seq.sortBy (fun strValue ->
            strValue.Replace("-","")
            |> int)

    let ones =
        regex4Ones |> getNumbers


    let twos =
        regex4Twos |> getNumbers

    {
        Ones = ones
        Twos = twos
    }



module CreateProductsInfoFile

open System.Text.RegularExpressions
open System
open System.IO
open System.Xml.Linq
open System.Xml.XPath
open System.Linq

type widthAlts =
    | Num of float
    | Str of string

let widths =
    seq[ 
        1.4  |> Num
        3.0  |> Num
        4.2  |> Num
        4.4  |> Num
        4.6  |> Num
        4.8  |> Num
        5.0  |> Num
        9.0  |> Num
        10.0 |> Num
        14.8 |> Num
        15.0 |> Num
        20.0 |> Num
        "5(1)" |> Str
        "10(1)" |> Str 
        "15(1)" |> Str
        "20(1)" |> Str
        25.0 |> Num
        30.0 |> Num
        40.0 |> Num
        50.0 |> Num
        60.0 |> Num
        70.0 |> Num
        80.0 |> Num
        90.0 |> Num
        100.0|> Num
        200.0|> Num
        400.0|> Num
    ]        
    |> Seq.map (fun tableHeader -> 
        match tableHeader with 
        | Num number -> number |> string
        | Str stringNumber -> stringNumber)

let importantHeaders =
    seq[
        "Product number(expand for Rev.)"
        "Functional designation"
        "Per TX"
    ]
let excelInfoAsText =
    let excelInfoAsDictionary =
        ExcelFunctions.getxlColumnsDictionary "C:\Users\egoljos\Documents\ScriptDocuments\RadioUnitsInformation.xlsx"

    excelInfoAsDictionary.DataRowWise
    |> Seq.map (fun dictionary -> 
        let widths =
            widths
            |> Seq.map (fun width ->
                dictionary.Item width + ",")
            |> String.concat ""
            |> fun str -> str.Substring(0,str.LastIndexOf(","))
        
        let infoPart1 =
            importantHeaders
            |> Seq.take 3
            |> Seq.map (fun name -> dictionary.Item name)

        let infoPart2 =
            let first =
                importantHeaders
                |> Seq.skip 3
                |> Seq.map (fun name -> dictionary.Item name)
            seq[widths]
            |> Seq.append first
        
        infoPart2
        |> Seq.append infoPart1
        |> Seq.map (fun col -> 
            col + "$")
        |> String.concat ""
        |> fun str -> str.Substring(0,str.LastIndexOf("$"))
        |> fun str -> str + "\n")
    |> String.concat ""

File.WriteAllText("C:\Users\egoljos\Documents\ScriptDocuments\RadioUnitsInformation.txt",excelInfoAsText)


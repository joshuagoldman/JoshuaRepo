module NewLATPRods2SiteInfoForScript


open System.IO
open System.Xml.Linq
open System.Xml.XPath
open System
open System.Text.RegularExpressions
open System.Linq


let finalString =
    let result =
        WriteNewProducts2Add.info
        |> Seq.map (fun prod ->
           String.Format(
            "R10L€{0}€{1}€LAT",
            prod.ProductNumber,
            prod.Name
           ))
        |> String.concat "\n"
    result

File.WriteAllText("C:\Users\egoljos\Documents\ScriptDocuments\Test.txt", finalString)
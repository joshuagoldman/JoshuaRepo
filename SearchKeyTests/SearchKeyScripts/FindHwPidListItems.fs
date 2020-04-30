module FindHwPidListItems

open System.IO
open System.Xml.Linq
open System.Xml.XPath
open System
open System.Text.RegularExpressions
open System.Linq

type HwPidListItems =
    {   
        ProdNumber : string
        Name : string
        Power : string
        FrequenceWidth : string
    }

let infoFile = File.ReadAllText("C:\Users\egoljos\Documents\ScriptDocuments\RadioUnitsInformation.txt")
let HwPidStream = File.Open("C:\Users\egoljos\Documents\Gitrepos\Nodetest\Datapackets\HWPidList\documents\HWPidList.xml", FileMode.OpenOrCreate)
let mutable xDoc = XDocument.Load(HwPidStream)
HwPidStream.Close()

let findRightFreqWidth ( widthsStr : string )  =
    let widthsCrosses = widthsStr.Split ','

    let widths = [| 1.4 ; 3.0 ; 4.2 ; 4.4 ; 4.6 ;
                    4.8 ; 5.0 ; 9.0 ; 10.0 ; 14.8 ;
                    15.0 ; 20.0 ; 5.0 ; 10.0 ; 15.0 ;
                    20.0 ; 25.0 ; 30.0 ; 40.0 ; 50.0 ;
                    60.0 ; 70.0 ; 80.0 ; 90.0 ; 100.0 ;
                    200.0 ; 400.0|]

    let anonRecSeq =
        Array.zip widthsCrosses [|0..widthsCrosses.Length - 1|]
        |> Array.map (fun (value, pos) -> {| Cross = value ;
                                             Value = widths.[pos]|})
    
    let foundSeq = 
       anonRecSeq
       |> Array.filter (fun anonRec -> anonRec.Cross <> "" && anonRec.Cross <> "-")

    let result =
        foundSeq
        |> function
            | sequence when sequence.Length <> 0 ->
                sequence
                |> Seq.tryFind (fun info -> 
                    info.Value = 5.0)
                |> function
                    | res when res.IsSome ->
                        res.Value.Value
                        |> string
                    | _ -> 
                        sequence.[0].Value 
                        |> string
            | _ -> ""
    result

let infoRecSeq = 
    let infoSeq =
        infoFile
        |> fun str ->
            str.Split '\n'
        |> Array.toSeq

    let infoSeqOfSeq =
        infoSeq
        |> Seq.map (fun str -> str.Split '$'
                               |> Array.toSeq)

    let result =
        infoSeqOfSeq
        |> Seq.map (fun infoSeq ->  infoSeq
                                    |> function
                                        | _ when infoSeq |> Seq.length >= 3 ->
                                            let productNumber =
                                                infoSeq |> Seq.item 0 |> fun str -> str.Trim().Replace(" ","")
                                    
                                            {   ProdNumber =  productNumber
                                                Name = infoSeq |> Seq.item 1  |> fun str -> str.Trim().Replace(" ","") ;
                                                Power = infoSeq |> Seq.item 2  |> fun str -> str.Trim().Replace(" ","") ;
                                                FrequenceWidth = findRightFreqWidth (infoSeq 
                                                                                    |> Seq.item 3
                                                                                    |> fun str -> 
                                                                                    str.Trim().Replace(" ",""))}

                                        | _ -> 
                                                { ProdNumber = "" ;
                                                  Name = "" ;
                                                  Power = "" ;
                                                  FrequenceWidth = ""})
    result

let ProdName name =
    Regex.IsMatch(name,"(?<! )(?<= |\n|^).*?([0-9]{4})?(?=[0-9]{1,3}B|B)")
    |> function
        | res when res = true ->
            Regex.Match(name,"(?<! )(?<= |\n|^).*?([0-9]{4})?(?=[0-9]{1,3}B|B)").Value.Replace(" ","").Trim()
        | _ -> name

let getPower (case : HwPidListItems) =

    let nn = case.Name
    let nj = (case.Name |> ProdName)
    let nh = case.ProdNumber

    let dataFoundThroughNumber =
        infoRecSeq
        |> Seq.tryFind (fun data -> data.ProdNumber.Replace(" ", "") = case.ProdNumber.Replace(" ", "").Trim())

    let dataFoundThroughName =
        infoRecSeq
        |> Seq.tryFind (fun data -> (case.Name |> ProdName) = (data.Name |> ProdName))
    
    None
    |>function
     | _ when dataFoundThroughNumber <> None -> 
            
            {case with Power = dataFoundThroughNumber.Value.Power}
     
     | _ when dataFoundThroughName <> None &&  
              dataFoundThroughNumber = None ->
             
             { case with Power = dataFoundThroughName.Value.Power}
     
     | _ -> case
    
let getFreqWidth (case : HwPidListItems) =

    let sss = case
    let dataFoundThroughNumber =
        infoRecSeq
        |> Seq.tryFind (fun data -> data.ProdNumber.Replace(" ", "").Trim() = case.ProdNumber.Replace(" ", "").Trim())

    let dataFoundThroughName =
        infoRecSeq
        |> Seq.tryFind (fun data -> (case.Name |> ProdName) = (data.Name |> ProdName))

    let nh = case.ProdNumber
    
    None
    |>function
     | _ when dataFoundThroughNumber <> None -> 
            
            {case with FrequenceWidth = dataFoundThroughNumber.Value.FrequenceWidth}
     
     | _ when dataFoundThroughName <> None &&  
              dataFoundThroughNumber = None ->
             
             { case with FrequenceWidth = dataFoundThroughName.Value.FrequenceWidth}
     
     | _ -> case
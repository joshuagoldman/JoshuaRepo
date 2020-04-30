module AddHwPidListPRTT

open System.IO
open System.Xml.Linq
open System.Xml.XPath
open System
open System.Text.RegularExpressions
open System.Linq
open FindHwPidListItems

let HwPidStream = File.Open("C:\Users\egoljos\Documents\Gitrepos\Nodetest\Datapackets\HWPidList\documents\HWPidList.xml", FileMode.OpenOrCreate)
let mutable xDoc = XDocument.Load(HwPidStream)
HwPidStream.Close() 

let int2Alphabet (num : int) = 
    
    let numFloat = float(num) - 1.0
    let factor = Operators.floor (numFloat/26.0)
    
    let newNum =  int(numFloat - factor * 26.0)  

    match newNum with
    | 0 -> "a" | 1 -> "b" | 2 -> "c" | 3 -> "d" | 4 -> "e"  | 5 -> "f" | 6 -> "g" | 7 -> "h"
    | 8 -> "i" | 9 -> "j" | 10 -> "k" | 11 -> "l" | 12 -> "m" | 13 -> "n" | 14 -> "o" | 15 -> "p"
    | 16 -> "q" | 17 -> "r" | 18 -> "s" | 19 -> "t" | 20 -> "u" | 21 -> "v" | 22 -> "w" | 23 -> "x"
    | 24 -> "y" | 25 -> "z" | _ -> ""

type ProductNumber =
    {
        Value : string
    }

type PortInfo = 
    {
        Band : string
        Power : string
        FrequencyWidth : string
    }

type PortLetter = 
    {
        Value : string
    }

type Port =
    {
        PortSeq : seq<PortInfo>
        Letter : PortLetter
    }

type HwPidListPRTTBase =
    {   
        Number : ProductNumber
        Name : string
        MarketName : string
        PortSeq : seq<Port>
    }

type Tags =
    {
        PrttSupp : {| Name : string ; Value : string|}
        LatSupp : {| Name : string ; Value : string|}
        LatCat : {| Name : string ; Value : string|}
    }

type FullElementType =
    {
        Base : HwPidListPRTTBase
        Tag : Tags
    }

type HWPidListLAT =
    | Base of HwPidListPRTTBase
    | FullElement of FullElementType
    | ProcedureDone of unit

let getPortSequence (info : WriteNewProducts2Add.NewProdsInfo) =
    
    let numOfPorts = 
        Regex.Match(info.Name.Trim(), "[1-9]\d")
        |> function
            | res when not(res.Success) -> 0
            | res -> res.Value.Substring(0,1) |> int

    let hwPidItems = 
        {
             ProdNumber = info.ProductNumber.Trim()
             Name = info.Name.Trim().Replace(" ","")
             Power = ""
             FrequenceWidth = ""
        }

    let bands =
        match info.Bands with
        | Some bands -> bands
        | _ -> seq[""]

    let power =
        ()
        |> function
            | _ when info.Power.IsSome ->
                info.Power.Value
            | _ -> 
                getPower hwPidItems
                |> fun x -> x.Power

    let freqWidth = 
        ()
        |> function
            | _ when info.Power.IsSome ->
                info.FrequencyWidth.Value
            | _ -> 
                getFreqWidth hwPidItems
                |> fun x -> x.FrequenceWidth

    let portBaseInfos = 
        bands
        |> Seq.map (fun band -> { Band = band  ;
                                  Power = power ;
                                  FrequencyWidth = freqWidth })

    let result =
        seq[1..numOfPorts]
        |> Seq.map (fun pairNum -> portBaseInfos
                                   |> fun portBaseInfo ->
                                        { PortSeq = portBaseInfo ;
                                          Letter = { Value = int2Alphabet pairNum} })
    result


let getFullElement =
    let result =
        WriteNewProducts2Add.info
        |> Seq.map (fun info -> 
            { Number = {Value = info.ProductNumber.Trim()} ;
                Name = info.Name.Trim() ;
                MarketName = "" ;
                PortSeq = getPortSequence info })

    result


let getTags =
    let prttSuppTag = {|Name = "prtt-supported" ; Value = "Yes"|}
    let latSuppTag = {|Name = "lat-supported" ; Value = "No"|}
    let categoryTag = {|Name = "lat-category" ; Value = "radio"|}
    {PrttSupp = prttSuppTag ; LatSupp = latSuppTag ; LatCat = categoryTag }

let makePortSeqToString (portInfos : seq<PortInfo>) =
    portInfos
    |> Seq.map (fun band -> band.Band + ";" +
                            band.Power + "W;" +
                            band.FrequencyWidth + "MHZ,")
    |> String.concat ""
    |> fun str -> str.Substring(0, str.LastIndexOf(','))

let getPortAttribs (portSeq : seq<Port>) =
    let portInfoAttrValue =
        makePortSeqToString (portSeq
                             |> Seq.item 0
                             |> fun x -> x.PortSeq)

    let letter (lettLowCase : string) =
        "RF_" + lettLowCase.ToUpper()

    portSeq
    |> Seq.map (fun port -> new XAttribute(XName.Get (letter port.Letter.Value),
                                                      portInfoAttrValue))

let getProdAttribs (fullElement : FullElementType) =
    
    let firstSeq =
        seq[new XAttribute(XName.Get "Number", fullElement.Base.Number.Value.Replace("161"," 161 ")) ;
        new XAttribute(XName.Get "Name", fullElement.Base.Name) ;
        new XAttribute(XName.Get "MarketName", fullElement.Base.MarketName) ;
        new XAttribute(XName.Get "RadioTestAllowed", "YES") ;
        new XAttribute(XName.Get "RequiresRadioTest", "YES")]
    
    let secSeq = getPortAttribs fullElement.Base.PortSeq

    Seq.append firstSeq secSeq

let createTagElement (tags : Tags) = 

    new XElement(XName.Get "Tags",
        new XElement(XName.Get "Tag",
                        XAttribute(XName.Get "Name", tags.PrttSupp.Name),
                        XAttribute(XName.Get "Value", tags.PrttSupp.Value)),
        new XElement(XName.Get "Tag",
                        XAttribute(XName.Get "Name", tags.LatSupp.Name),
                        XAttribute(XName.Get "Value", tags.LatSupp.Value)),
        new XElement(XName.Get "Tag",
                        XAttribute(XName.Get "Name", tags.LatCat.Name),
                        XAttribute(XName.Get "Value", tags.LatCat.Value)))


let getXmlTree (fullElement : FullElementType) = 
    let element = 
        new XElement(XName.Get "Product", getProdAttribs fullElement,
            createTagElement fullElement.Tag)

    let xPath = 
        "*//Product[@Number = '" + fullElement.Base.Number.Value.Replace("161"," 161 ") + "']"
    ()
    |> function
        | _ when xDoc.XPathSelectElements("*//Product[@Number = '" + fullElement.Base.Number.Value.Replace("161"," 161 ") + "']").Count() > 0 ->
            let existingELement =
                xDoc.XPathSelectElements(xPath) 
                |> Seq.item 0
            
            ()
            |> function
                | _ when existingELement.Attributes().Count() < 4 ->
                    let newAttributes =
                        element.Attributes()
                        |> Seq.append (existingELement.Attributes())
                        |> Seq.distinctBy (fun attr -> attr.Name.LocalName)

                    let newChildren =
                        let suppLatEl = existingELement.XPathSelectElements("*//Tag[@Name='lat-supported']")

                        suppLatEl
                        |> function
                            | res when res |> Seq.length <> 0 ->
                                let newLatSupportVal =
                                    res
                                    |> Seq.item 0
                                    |> fun el ->
                                        {|
                                            Name = el.FirstAttribute.Value
                                            Value = el.Attributes().ElementAt(1).Value
                                        |}
                                           
                                { fullElement.Tag with LatSupp = newLatSupportVal}
                                |> createTagElement
                                |> fun x -> seq[x]
                            | _ -> 
                                element.Elements()
                    let newElement =
                        new XElement(XName.Get existingELement.Name.LocalName,
                            newAttributes,
                            newChildren)

                    xDoc.XPathSelectElement(xPath).ReplaceWith(newElement)
                | _ -> ()
                
        | _ -> 
            xDoc.XPathSelectElements("*//Product")
            |> Seq.map (fun el -> el.FirstAttribute.Value)
            |> Seq.append [fullElement.Base.Number.Value]
            |> Seq.sortBy (fun prodNum -> 
                Regex.Match(prodNum.Replace(" ",""), "(?<=[aA-zZ]{3})(?![aA-zZ]).*?(?=[aA-zZ]|$)").Value
                |> fun str -> str.Replace("/","")
                |> fun str -> str.Substring(0,1) + "." + str.Substring(1,str.Count() - 1)
                |> float) 
            |> fun sequence ->
                Seq.zip sequence [0..sequence |> Seq.length |> fun x -> x - 1]
                |> Seq.tryFind (fun  (prodNum,_) -> prodNum = fullElement.Base.Number.Value)
                |> function
                    | res when res.IsSome ->
                        res.Value
                        |> fun (_,pos) ->
                            sequence
                            |> Seq.item(pos - 1)
                    | _ -> 
                        sequence
                        |> Seq.item 0
            |> fun elAttrPriorToSelf ->
                xDoc.XPathSelectElement("*//Product[@Number = '" + elAttrPriorToSelf + "']").AddAfterSelf(element)
    xDoc.Save("C:\Users\egoljos\Documents\ScriptDocuments\HwPidListTest.xml")
    let finalString  = xDoc.XPathSelectElement(xPath).ToString()
    File.AppendAllText("C:\Users\egoljos\Documents\ScriptDocuments\TestLATProds.txt",finalString + "\n\n")
                        

let rec msgFunc (state : HWPidListLAT) =
    match state with
    | Base(hwPidBase) ->
        let fullElementType = {Base = hwPidBase ; Tag = getTags }
        FullElement(fullElementType )

    | FullElement(fullElementType) -> 
        getXmlTree fullElementType
        ProcedureDone()

    | ProcedureDone() ->
        ProcedureDone()

    |> function
       | postState when postState = ProcedureDone() -> ignore
       | postState -> msgFunc postState 

File.WriteAllText("C:\Users\egoljos\Documents\ScriptDocuments\TestLATProds.txt","")
getFullElement
|> Seq.map (fun case -> Base(case))
|> Seq.iter (fun case -> msgFunc case |> ignore)


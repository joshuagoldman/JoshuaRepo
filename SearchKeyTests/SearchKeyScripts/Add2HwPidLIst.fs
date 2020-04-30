namespace SearchKeyRep

open System.IO
open System.Xml.Linq
open System.Xml.XPath
open System
open System.Text.RegularExpressions
open System.Linq

type ProductNumber =
    {
        Value : string
    }

type HwPidListLATBase =
    {   
        Number : string
        Name : string
        MarketName : string
    }

type Tags =
    {
        LatCat : {| Name : string ; Value : string|}
        LatSupp : {| Name : string ; Value : string|}
    }

type PositionInfo =
    {
        PosName : string
        PosValue : string
    }

type HWPidListLAT =
    | ProdNumberOnly of ProductNumber
    | Base of ProductNumber * HwPidListLATBase
    | FullElement of ProductNumber * HwPidListLATBase * Tags
    | Complete of HwPidListLATBase * Tags * PositionInfo
    | ProcedureDone of unit


module Add2HwPidLIst =
    
    //let infoFile = File.ReadAllText("C:\Users\jogo\Documents\jogo\Ericsson\Radio_Units_Info.txt")
    let HwPidStream = File.Open("C:\Users\egoljos\Documents\Gitrepos\Nodetest\Datapackets\HWPidList\documents\HWPidList.xml", FileMode.OpenOrCreate)
    let mutable xDoc = XDocument.Load(HwPidStream)
    HwPidStream.Close() 

    //let infoRecSeq = 
    //    infoFile
    //    |> fun str -> str.Split '''
    //    |> Seq.map (fun str -> str.Split ';')
    //    |> Seq.map (fun infoSeq ->  infoSeq
    //                                    |> function
    //                                        | _ when infoSeq.Length >= 3 ->
                                        
    //                                            { Number = infoSeq.[2] |> fun str -> str.Trim() ;
    //                                              Name = infoSeq.[1] |> fun str -> str.Trim().Replace(" ","") ;
    //                                              MarketName = infoSeq.[1] |> fun str -> str.Trim()}

    //                                        | _ -> 
    //                                            { Number = "" ;
    //                                              Name = "" ;
    //                                              MarketName = ""})

    let newProducts =
        WriteNewProducts2Add.info
        |> Seq.map (fun infoSeq ->  
                { 
                Number = infoSeq.ProductNumber |> fun str -> str.Trim() ;
                Name = infoSeq.Name |> fun str -> str.Trim().Replace(" ","") ;
                MarketName = infoSeq.Name |> fun str -> str.Trim()
            })
    

    let extractNumber (str : string) =
        Regex.Match(str, "(?<= ).*")
        |> function
           | res when res.Value = "" -> 0
           | res when Regex.Match(res.Value, "[a-zA-Z]").Success -> 0
           | res -> res.Value.Replace(" ","").Replace("/", "") |> int

    let evaluateElement (currProdNumber : string) =
        xDoc.XPathSelectElements("*//*")
        |> Seq.map (fun element -> element.Attributes()
                                      |> fun x -> x.ToArray()
                                      |> function
                                          | infoSeq when (infoSeq.Length <= 3) ->
                                            
                                            {|Element = element ; ExtrNumber = 0|}

                                          | infoSeq when (
                                                            infoSeq 
                                                            |> Array.item 0 
                                                            |> fun x -> x.Value.Substring(0, 3)
                                               
                                                          ) = currProdNumber.Substring(0, 3) ->
                                                            
                                                                {|Element = element ; ExtrNumber = extractNumber infoSeq.[0].Value|}

                                          | _ -> 

                                            {|Element = element ; ExtrNumber = 0|})


    let getBase (str : string) =
        newProducts
        |> Seq.tryFind (fun info -> info.Number.Replace(" ", "") = str.Replace(" ", ""))
        |> function
           | res when res.IsNone -> Base({Value = str}, {Number = "" ; Name = "" ; MarketName = ""})
           | res -> Base({Value = str}, {Number = res.Value.Number ; Name = res.Value.Name; MarketName = res.Value.Number})

    let getFullElement =
        let categoryTag = {|Name = "lat-category" ; Value = "radio"|}
        let supportedTag = {|Name = "lat-supported" ; Value = "Yes"|}
        {LatCat = categoryTag ; LatSupp = supportedTag}
   

    let getComplete (currProdNum : string) =
        evaluateElement currProdNum
        |> fun sequence ->  Seq.append [{|Element = new XElement(XName.Get "Chosen", "Chosen")  ; ExtrNumber = extractNumber currProdNum|}] sequence
        |> Seq.filter (fun anRec -> anRec.ExtrNumber <> 0)
        |> Seq.sortBy (fun info -> info.ExtrNumber) 
        |> fun sortedSeq -> 
            {| Info = sortedSeq |> Seq.toArray ; Index = sortedSeq 
                                                         |> Seq.tryFindIndex (fun info -> info.ExtrNumber = extractNumber currProdNum)|}
        |> function
           | res when res.Index = None -> {PosName = "" ; PosValue = "" }
           | res -> {PosName = res.Info.[res.Index.Value - 1].Element.FirstAttribute.Name.LocalName ;
                     PosValue = res.Info.[res.Index.Value - 1].Element.FirstAttribute.Value }
    
    let createXElement (hwpidBase : HwPidListLATBase)
                       (tags : Tags)= 
        new XElement(XName.Get "Product", 
                     XAttribute(XName.Get "Number", hwpidBase.Number.Replace("161"," 161 ")),
                     XAttribute(XName.Get "Name", hwpidBase.Name),
                     XAttribute(XName.Get "RadioTestAllowed", "Yes"),
                        new XElement(XName.Get "Tags",
                            new XElement(XName.Get "Tag",
                                         XAttribute(XName.Get "Name", tags.LatCat.Name),
                                         XAttribute(XName.Get "Value", tags.LatCat.Value)),
                            new XElement(XName.Get "Tag",
                                         XAttribute(XName.Get "Name", tags.LatSupp.Name),
                                         XAttribute(XName.Get "Value", tags.LatSupp.Value))))

    

    let performProcedure (hwPidBase : HwPidListLATBase)
                         (tags : Tags)
                         (posInfo : PositionInfo) =
        let element = 
            tags
            |> createXElement hwPidBase

        ()
        |> function
            | _ when xDoc.XPathSelectElements("*//Product[@Number = '" + hwPidBase.Number.Replace("161"," 161 ") + "']").Count() > 0 ->
                ()
            | _ -> 
                xDoc.XPathSelectElements("*//Product")
                |> Seq.map (fun el -> el.FirstAttribute.Value)
                |> Seq.append [hwPidBase.Number]
                |> Seq.sortBy (fun prodNum -> 
                    Regex.Match(prodNum.Replace(" ",""), "(?<=[aA-zZ]{3})(?![aA-zZ]).*?(?=[aA-zZ]|$)").Value
                    |> fun str -> str.Replace("/","")
                    |> fun str -> str.Substring(0,1) + "." + str.Substring(1,str.Count() - 1)
                    |> float) 
                |> fun sequence ->
                    Seq.zip sequence [0..sequence |> Seq.length |> fun x -> x - 1]
                    |> Seq.tryFind (fun  (prodNum,_) -> prodNum = hwPidBase.Number)
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
        File.AppendAllText("C:\Users\egoljos\Documents\ScriptDocuments\TestLATProds.txt",element.ToString() + "\n\n")

    let rec msgFunc (state : HWPidListLAT) =
        match state with
        | ProdNumberOnly(str) -> 
            getBase str.Value
        | Base(prodNum, hwPidBase) ->
            FullElement(prodNum, hwPidBase, getFullElement)

        | FullElement(prodNum, hwPidBase, tag) -> 
            Complete(hwPidBase, tag , getComplete prodNum.Value)

        | Complete(hwPidBase, tag, posInfo) ->
            performProcedure hwPidBase tag posInfo
            ProcedureDone()

        | ProcedureDone() ->
            ProcedureDone()

        |> function
           | postState when postState = ProcedureDone() -> ignore
           | postState -> msgFunc postState 
    
    File.WriteAllText("C:\Users\egoljos\Documents\ScriptDocuments\TestLATProds.txt","")
    newProducts
    |> Seq.map (fun info -> info.Number)
    |> Seq.map (fun case -> ProdNumberOnly({Value = case}))
    |> Seq.iter (fun case -> msgFunc case |> ignore)


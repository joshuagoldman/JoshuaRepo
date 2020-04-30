module AddToConfigKeyList

open System.IO
open System.Xml.Linq
open System.Xml.XPath
open System
open System.Text.RegularExpressions
open System.Linq

let prodInfos =
    WriteNewProducts2Add.info

let configKeyDefStream = File.Open("C:\Users\egoljos\Documents\Gitrepos\Nodetest\Datapackets\RBS6000Definitions\documents\ConfigkeyDefinitions2.xml", FileMode.OpenOrCreate)
let mutable xDoc = XDocument.Load(configKeyDefStream)
configKeyDefStream.Dispose()

type ParentWithChild = {
    Child : XElement
    Parent : XElement
}

let getData ( prodInfo : WriteNewProducts2Add.NewProdsInfo ) =

    let configKeyChildParent =
        let xpath =
            String.Format(
                "*//ConfigEntry[@Data='{0}']",
                prodInfo.ProductNumber.
                    Replace("161", " 161 ").
                    Replace("901", " 901 ").
                    Replace("118", " 118 ")
            )

        let element = xDoc.XPathSelectElement(xpath)

        let parent = element.Parent

        {
            Child = element
            Parent = parent
        }

    let tableKey =
        configKeyChildParent.Parent.Attributes().ElementAt(1).Value
        |> char

    let entryKey = 
        configKeyChildParent.Child.FirstAttribute.Value
        |> char

    let triggerValues =
        let xPath =
            "*//DynamicField[@Definition='RadioUnit' and @TriggerValue]"

        let element =
            xDoc.XPathSelectElement(xPath)

        let resultValues =
            element.Attributes().ElementAt(2).Value
            |> fun str -> str.Replace(",","").Replace(" ","")
            |> Seq.map (fun characater -> characater)
        resultValues

    let standardConfigKey =
        "211000100100000000203000000000000000000000000000FP01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000oi401B1100001000000000000000000000000000000000000000000000000000000000000"
        |> Seq.map (fun character -> character)

    let tableKeyPosition =
        Seq.zip standardConfigKey [0..standardConfigKey |> Seq.length |> fun x  -> x - 1]
        |> Seq.tryFind (fun (key,pos) ->
            triggerValues
            |> Seq.exists (fun character -> 
                character = key))
            |> function
                | res when res.IsSome ->
                    res.Value
                    |> fun (_,pos) -> pos
                | _ -> 0

    let rbsSoftwareLteKey =
        let xPath =
            "*//ConfigItemDefinition[@Name='SoftwareTrackLteDef']"

        let softwareElements =
            xDoc.XPathSelectElement(xPath).Elements().ElementAt(0).Elements()
        
        let swRegex =
            new Regex("(?<![aA-zZ])[0-9]{2}.*(?=Q).*")

        let filteredValues =
            softwareElements
            |> Seq.map (fun swElement ->
                let sw = 
                    swElement.Attributes().ElementAt(1).Value
                let result =
                    swRegex.Match(sw)
                    |> function 
                        | res when res.Success ->
                            (swElement, res.Value.Replace("Q","").
                                                  Replace("_",".").
                                                  Replace("C",""))
                            |> Some
                        | _ -> None 
                result)
            |> Seq.choose id
        
        let result =
            filteredValues
            |> Seq.map (fun (swELement,swName) ->
                let swKey =
                    swELement.Attributes().ElementAt(0).Value
                {|
                    SwNumber = swName |> float
                    SwKey = swKey
                |}
                )
            |> Seq.map (fun info -> 

                (info.SwKey,info.SwNumber))
            |> Seq.maxBy (fun (_,number) ->
                number)
            |> fun (swKey,_) -> swKey

        result

    let swPosition =
        let xPath =
            "*//ConfigItem[@Name='SoftwareTrackLte']"

        let result =
            xDoc.XPathSelectElement(xPath).FirstAttribute.Value
            |> int
            |> fun num -> num - 1

        result

    let newConfigKey =
        Seq.zip standardConfigKey [0..standardConfigKey |> Seq.length |> fun x  -> x - 1]
        |> Seq.map (fun (keyPosValue,pos) ->
            ()
            |> function
                | _ when pos = tableKeyPosition  ->
                    tableKey
                | _ when pos = tableKeyPosition + 1 ->
                    entryKey
                | _ when pos = swPosition ->
                    rbsSoftwareLteKey
                    |> char
                | _ ->
                    keyPosValue
            )
        |> Seq.map (fun c -> c |> string)
        |> String.concat ""

    newConfigKey
    
//let PRTTlISTStream = File.Open("C:\Users\egoljos\Documents\Gitrepos\nodetest\Datapackets\ConfigKeyList\documents\PRTTList.xml", FileMode.OpenOrCreate)
let mutable xDOcPrttList = XDocument.Load("C:/Users/egoljos/Documents/Gitrepos/nodetest/Datapackets/ConfigKeyList/documents/PRTTList.xml")

let writePrttListElement ( prttListDoc : XDocument )
                         ( newProd : WriteNewProducts2Add.NewProdsInfo ) 
                           configKey=
    let newElement =
        new XElement(XName.Get "Product", 
            new XAttribute(XName.Get "Number", newProd.ProductNumber.Replace("161"," 161 ")),
            new XElement(XName.Get "Version",
                new XAttribute(XName.Get "State", "*"),
                new XAttribute(XName.Get "ConfigKey", configKey)))

    let productsAll =
        prttListDoc.XPathSelectElements("*//Product")
        |> Seq.map (fun el -> 
            el.FirstAttribute.Value)
        |> Seq.append [newProd.ProductNumber]
        |> Seq.sortBy (fun prodNum -> 
            Regex.Match(prodNum.Replace(" ",""), "(?<=[aA-zZ]{3})(?![aA-zZ]).*?(?=[aA-zZ]|$)").Value
            |> fun str -> str.Replace("/","")
            |> fun str -> str.Substring(0,1) + "." + str.Substring(1,str.Count() - 1)
            |> float) 

    let ProdNumPriorToSelf =
        Seq.zip productsAll [0..productsAll |> Seq.length |> fun x -> x - 1]
        |> Seq.tryFind (fun  (prodNum,_) -> prodNum = newProd.ProductNumber)
        |> function
            | res when res.IsSome ->
                res.Value
                |> fun (_,pos) ->
                    productsAll
                    |> Seq.item(pos - 1)
            | _ -> 
                productsAll
                |> Seq.item 0

    let elementPriorToSelf =
        prttListDoc.Root.XPathSelectElement("*//Product[@Number = '" + ProdNumPriorToSelf + "']")

    elementPriorToSelf.AddAfterSelf(newElement)
    prttListDoc.Save("C:\Users\egoljos\Documents\ScriptDocuments\PRTTListTest.xml")

   

WriteNewProducts2Add.info
|> Seq.skip 1
|> Seq.map (fun newProd -> 
    let newConfigKey =
        newProd 
        |> getData

    newConfigKey
    |> writePrttListElement xDOcPrttList newProd
    
    newConfigKey + "\n\n")
|> String.concat ""
|> fun resString -> 
    File.WriteAllText("C:\Users\egoljos\Documents\ScriptDocuments\Test.txt",resString)

        


        


module AddConfigKeyDefinitions 

open System.IO
open System.Xml.Linq
open System.Xml.XPath
open System
open System.Text.RegularExpressions
open System.Linq


type ConfigKeyInfo =
    {
        Band : string
        Label : string
        Data : string
        ProductType : string
        ProductName : string
        Description : string
    }

type configKeyInfoWithLetter =
    {
        Info : ConfigKeyInfo
        Letter : string
    }

let int2Alphabet (num : int) = 
    
    let numFloat = float(num) - 1.0
    let factor = Operators.floor (numFloat/26.0)
    
    let newNum =  int(numFloat - factor * 26.0)  

    match newNum with
    | 0 -> "a" | 1 -> "b" | 2 -> "c" | 3 -> "d" | 4 -> "e"  | 5 -> "f" | 6 -> "g" | 7 -> "h"
    | 8 -> "i" | 9 -> "j" | 10 -> "k" | 11 -> "l" | 12 -> "m" | 13 -> "n" | 14 -> "o" | 15 -> "p"
    | 16 -> "q" | 17 -> "r" | 18 -> "s" | 19 -> "t" | 20 -> "u" | 21 -> "v" | 22 -> "w" | 23 -> "x"
    | 24 -> "y" | 25 -> "z" | _ -> ""

let possibleConfigKeyEntries =
    let intSequence = seq[0..9]

    let numbersStringSequence =
        intSequence
        |> Seq.map (fun num -> num |> string)
        
    let alphabetIntSequence = seq[1..26]

    let alphabetStringSequence =
        alphabetIntSequence
        |> Seq.map (fun num -> num |> int2Alphabet)

    let alphabetCapitalStringSequence =
        alphabetIntSequence
        |> Seq.map (fun num -> 
            num 
            |> int2Alphabet
            |> fun str -> str.ToUpper())

    let finalSequence =
        alphabetStringSequence
        |> Seq.append alphabetCapitalStringSequence
        |> Seq.append numbersStringSequence

    Seq.zip finalSequence [0..finalSequence |> Seq.length |> fun x -> x - 1]
    |> Seq.map (fun (key,position) ->
        {|
            Key = key
            Position = position
        |})

let getconfigKeyRegex sequence =
    let finalRegeXString =
        sequence
        |> Seq.map (fun key -> key + "|")
        |> String.concat ""
        |> fun str -> "(" + str.Substring(0,str.LastIndexOf("|")) + ")"
        |> fun str -> new Regex(str)

    finalRegeXString

type ConfigKeyTable = {
    Id : int
    SelectionValue : string
}

type configKeyPair = {
    New : string
    Old : string
}

type TablePair = {
    NewTable : ConfigKeyTable
    OldTable : ConfigKeyTable
}

type AddKeyOptions =
    | NewTable of TablePair
    | ExistingTable of ConfigKeyTable * configKeyPair

type TableInfo = {
    Table : ConfigKeyTable
    Keys : seq<string>
}

let  getKeySequence ( element : XElement ) =
    let allElements =
        element.XPathSelectElements("ConfigEntry")

    allElements
    |> Seq.map (fun el -> 
        el.FirstAttribute.Value)

let configKeyDefStream = File.Open("C:\Users\egoljos\Documents\Gitrepos\Nodetest\Datapackets\RBS6000Definitions\documents\ConfigkeyDefinitions2.xml", FileMode.OpenOrCreate)
let mutable xDoc = XDocument.Load(configKeyDefStream)

let allTables ( document : XDocument ) =
    document.XPathSelectElements("*//ConfigTable")
    |> Seq.filter (fun el -> 
        el.FirstAttribute.Value.Contains("RRUandAIRTable"))
    |> Seq.map (fun el -> 
        let table = {
            Id = el.FirstAttribute.Value.Replace("RRUandAIRTable#","") |> int
            SelectionValue = el.Attributes().ElementAt(1).Value
        }
        let keys = el |> getKeySequence

        let info =
            {
                Table = table
                Keys = keys
            }  

        info
    )

let configKeyToAddSetting ( document : XDocument )  =
    let lastTableIndex =
        document
        |> allTables
        |> Seq.length
        |> fun x -> x - 1

    let latestTable ( document : XDocument ) =
        document
        |> allTables
        |> Seq.sortBy (fun table -> table.Table.Id)
        |> Seq.item lastTableIndex
    
    let newConfigKeyToAdd ( document : XDocument ) =
        document
        |> latestTable
        |> fun tbl ->
            tbl.Keys
            |> getconfigKeyRegex
            |> fun regEx ->
                possibleConfigKeyEntries
                |> Seq.filter(fun entry ->
                    regEx.IsMatch(entry.Key) = false)
                |> function
                    | res when res |> Seq.length <> 0 -> 
                        let newEntry =
                            res
                            |> Seq.item 0
                        let oldEntry =
                            possibleConfigKeyEntries
                            |> Seq.item(newEntry.Position - 1)

                        let configKeyPair =
                            {
                                New = newEntry.Key
                                Old = oldEntry.Key
                            }

                        (tbl.Table,configKeyPair)
                        |> ExistingTable
                    | _ -> 
                        let newSelectionValue =
                            possibleConfigKeyEntries
                            |> Seq.find (fun entry ->
                                entry.Key = tbl.Table.SelectionValue)
                            |> fun entry ->
                                possibleConfigKeyEntries
                                |> Seq.item(entry.Position - 1)
                                |> fun entry -> entry.Key

                        let newTable =
                            { tbl.Table with SelectionValue = newSelectionValue
                                             Id = tbl.Table.Id + 1}
                        let newTablePair =
                            {
                                NewTable = newTable
                                OldTable = tbl.Table
                            }

                        newTablePair
                        |> NewTable
                    

    newConfigKeyToAdd document

let  getBands bandsOpt =
    match bandsOpt with
    | Some bands ->
        bands
        |> Seq.map (fun band -> band + ";")
        |> String.concat ""
        |> fun x -> x.Substring(0,x.LastIndexOf(";"))
    | _ -> ""

let ProdName name =
    Regex.IsMatch(name,"(?<! )(?<= |\n|^).*?([0-9]{4})?(?=[0-9]{1,3}B|B)")
    |> function
        | res when res = true ->
            Regex.Match(name,"(?<! )(?<= |\n|^).*?([0-9]{4})?(?=[0-9]{1,3}B|B)").Value.Replace(" ","").Trim()
        | _ -> name

let getProdType ( prodNumber : string ) =
    ()
    |> function
        | _ when prodNumber.ToUpper().Contains("KRC") -> "Rrus"
        | _ -> "Air"

let addNewEntry ( newProdsInfo : ConfigKeyInfo )
                ( document : XDocument ) =
    let setting =
        document
        |> configKeyToAddSetting
    match setting with
    | AddKeyOptions.NewTable tablePair ->
        let oldTableXpath =
            String.Format(
                "*//ConfigTable[@Name='RRUandAIRTable#{0}' and @SelectionValue='{1}']",
                tablePair.OldTable.Id |> string,
                tablePair.OldTable.SelectionValue
            )

        let zeroEntry =
            new XElement(XName.Get "ConfigEntry" ,
                XAttribute(XName.Get "Key", "0"),
                XAttribute(XName.Get "Data", "NotUsed"),
                XAttribute(XName.Get "Label", "Not Used"))

        let newTableName =
            "RRUandAIRTable#" + (tablePair.NewTable.Id |> string)

        let newTableSkeleton =
            seq[
                XAttribute(XName.Get "Name", newTableName)
                XAttribute(XName.Get "SelectionValue", tablePair.NewTable)
            ]

        let newEntry =
            new XElement(XName.Get "ConfigEntry" ,
                XAttribute(XName.Get "Key",  "1"),
                XAttribute(XName.Get "Data", newProdsInfo.Data),
                XAttribute(XName.Get "Band", newProdsInfo.Band),
                XAttribute(XName.Get "Label", newProdsInfo.Label),
                XAttribute(XName.Get "ProductType", newProdsInfo.ProductType),
                XAttribute(XName.Get "ProductName", newProdsInfo.ProductName),
                XAttribute(XName.Get "Description", newProdsInfo.Description))

        let newTable =
            new XElement(XName.Get "ConfigTable",
                newTableSkeleton,
                zeroEntry,
                newEntry)

        xDoc.XPathSelectElement(oldTableXpath).AddAfterSelf(newTable)
        xDoc.Save("C:\Users\egoljos\Documents\ScriptDocuments\ConfigKeyDefinitions2Test.xml")


    | AddKeyOptions.ExistingTable(table,conKeyPair) ->
        let tablexPath =
            String.Format(
                "*//ConfigTable[@Name='RRUandAIRTable#{0}' and @SelectionValue='{1}']",
                table.Id |> string,
                table.SelectionValue
            )

        let oldEntryXpath =
            String.Format(
                "ConfigEntry[@Key='{0}']",
                conKeyPair.Old
            )

        let newEntry =
            new XElement(XName.Get "ConfigEntry" ,
                XAttribute(XName.Get "Key",  conKeyPair.New),
                XAttribute(XName.Get "Data", newProdsInfo.Data),
                XAttribute(XName.Get "Band", newProdsInfo.Band),
                XAttribute(XName.Get "Label", newProdsInfo.Label),
                XAttribute(XName.Get "ProductType", newProdsInfo.ProductType),
                XAttribute(XName.Get "ProductName", newProdsInfo.ProductName),
                XAttribute(XName.Get "Description", newProdsInfo.Description))

        let test1 =  xDoc.XPathSelectElement(tablexPath)
        let test2 =  xDoc. XPathSelectElement(oldEntryXpath)
        let sasa = 
            xDoc.XPathSelectElement(tablexPath).
                XPathSelectElement(oldEntryXpath)

        xDoc.XPathSelectElement(tablexPath).
            XPathSelectElement(oldEntryXpath).
            AddAfterSelf(newEntry)
        xDoc.Save("C:\Users\egoljos\Documents\ScriptDocuments\ConfigKeyDefinitions2Test.xml")

let elInfo =
    WriteNewProducts2Add.info
    |> Seq.skip 1
    |> Seq.map (fun newProd ->

        { 
            Band = newProd.Bands |> getBands
            Label = newProd.Name
            Data = newProd.ProductNumber.Replace("161"," 161 ")
            ProductType = newProd.ProductNumber |> getProdType
            ProductName = newProd.Name |> ProdName
            Description = ""
        })

elInfo
|> Seq.iter(fun newProdInfo -> xDoc
                               |> addNewEntry newProdInfo)
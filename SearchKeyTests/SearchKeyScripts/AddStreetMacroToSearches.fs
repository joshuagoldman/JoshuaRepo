module AddStreetMacroToSearches

open System.Text.RegularExpressions
open System
open System.IO
open System.Xml.Linq
open System.Xml.XPath

type LogidExcelTable = {
    Logid : string
    Condition : seq<string>
}

type StreetMacroOption =
    | NotIncluded of seq<string>
    | Included of seq<string> 

type CriteriaType =
    | Divided of seq<seq<string>>
    | Single

type Criteria = {
    Name : string
    Products : CriteriaType
}

let xmlFile =
    XDocument.Load("C:\Users\egoljos\Documents\Gitrepos\LogAnalyzer\Ericsson.AM.LogAnalyzer\EmbeddedCriteria\RBS6000\Aftermarket\HWLogCriteria.xml")

let relevantCriteriasAsElements =

    let result =
        xmlFile.XPathSelectElements("//*")
        |> Seq.filter (fun el -> el.Name.LocalName.Contains("SearchKey"))
        |> Seq.filter (fun el -> el.HasAttributes)
        |> Seq.filter (fun el -> el.FirstAttribute.Value.Contains("1/-37"))

    result 
 
let excelInformation =
    let result =    
        ExcelFunctions.getxlColumns "C:\Users\egoljos\Documents\ScriptDocuments\1_37_EXCEL_1.xlsx"
    result

let product str =
    "ProductNumber=\"" + str + "\" RState=\"*\" />
    "
    
let getproductsRegex = 
    new Regex("[Kk].*?(\*|(?= ))")

let productChunksRegex = 
    new Regex("(IF Product Number = )(\n|.)*?( THEN)")

let isDivided strSeq =
    let regexExpr = 
        new Regex("IF Product Number")
    
    let result =
        strSeq
        |> Seq.tryFind (fun str ->
            regexExpr.Matches(str).Count
            |> function
                | res when res = 2 -> true
                | _ -> false )
    result
            
let streetMacroRegex = new Regex("(KRK1010)(1|2|3|5)\*")
let hasStreetMacro input =
    streetMacroRegex.IsMatch(input)


let getProducts input =
    let regexExpr = new Regex("(KRK1010)(1|2|3|5)\*")

    let matches = regexExpr.Matches(input)

    [0..matches.Count - 1]
    |> List.map (fun pos ->
         matches.[pos].Value
         |> product)
    |> String.concat ""


let createConds row =
    let conditions = 
        seq[
            row |> Seq.item 9
            row |> Seq.item 10
            row |> Seq.item 12
            row |> Seq.item 13
        ]
    {
        Logid = row |> Seq.item 1
        Condition = conditions 
    }

let singleorDivided row =
    row.Condition
    |> function
        | cond when (cond |> isDivided).IsSome ->
            cond
            |> String.concat ""
            |> fun x ->
                let matches = productChunksRegex.Matches(x)
                [0..matches.Count - 1]
                |> Seq.map (fun pos -> matches.[pos].Value)
            |> Seq.map (fun matchValue ->
                let sequenceProds input =
                    let prodMatches = getproductsRegex.Matches(input)
                    [0..prodMatches.Count - 1]
                    |> List.map (fun pos -> prodMatches.[pos].Value)
                    |> List.toSeq
                matchValue
                |> sequenceProds)
            |> Divided
        | _ -> Single
    |> fun tp ->
        {
            Name = row.Logid
            Criteria.Products = tp
        }

let sequenceOfCriteriaTypeOption =
    excelInformation.DataColumnWise
    |> function
        | info when info |> Seq.length > 14 -> 
            excelInformation.DataRowWise
            |> Seq.map (fun row -> 
                row
                |> createConds
                |> singleorDivided)
            |> Seq.skip 2
            |> Some
        | _ -> None

let newProdsSequence =
    seq[
        {| Prod = "KRK 10101.*" ; Rstate = "*" |}
        {| Prod = "KRK 10102.*" ; Rstate = "*" |}
        {| Prod = "KRK 10103.*" ; Rstate = "*" |}
        {| Prod = "KRK 10107.*" ; Rstate = "*" |}
        {| Prod = "KRK 10105.*" ; Rstate = "*" |}
    ]

type prodTranslation = {
    Abbreviation : string
    Products : seq<{| Prod : string ; Rstate : string |}>
}

let translations =
    seq[
        {
            Abbreviation = "KDV12762*"
            Products =
                seq[
                    {| Prod = "KDV 127 620/11" ; Rstate = "*" |}
                    {| Prod = "KDV 127 621/11" ; Rstate = "*" |}
                ]
        }
        {
            Abbreviation = "KDU13784*"
            Products =
                seq[
                    {| Prod = "KDU 137 847/11" ; Rstate = "*" |}
                    {| Prod = "KDU 137 848/11" ; Rstate = "*" |}
                ]
        }
        {
            Abbreviation = "KDU1370053*"
            Products =
                seq[
                    {| Prod = "KDU 137 0053/31" ; Rstate = "*" |}
                ]
        }
        {
            Abbreviation = "KDU137925*"
            Products =
                seq[
                    {| Prod = "KDU 137 925/41" ; Rstate = "*" |}
                    {| Prod = "KDU 137 925/31" ; Rstate = "*" |}
                ]
        }
        {
            Abbreviation = "KRK10101*"
            Products =
                seq[
                    {| Prod = "KRK 101 01.*" ; Rstate = "*" |}
                ]
        }
        {
            Abbreviation = "KRK10102*"
            Products =
                seq[
                    {| Prod = "KRK 101 02.*" ; Rstate = "*" |}
                ]
        }
        {
            Abbreviation = "KRK10103*"
            Products =
                seq[
                    {| Prod = "KRK 101 03.*" ; Rstate = "*" |}
                ]
        }
        {
            Abbreviation = "KRK10105*"
            Products =
                seq[
                    {| Prod = "KRK 101 05.*" ; Rstate = "*" |}
                ]
        }
        {
            Abbreviation = "KRK10107*"
            Products =
                seq[
                    {| Prod = "KRK 101 07.*" ; Rstate = "*" |}
                ]
        }
    ]

let xmlLevels =
    seq[
        "SearchSettings"
        "Information"
    ]

let rec xmlDiging ( fatherElement : XElement )
                    names
                  ( newValues : seq<{| Prod : string ; Rstate : string |}> ) = 

    let changeDate ( element : XElement ) =
        let newValue = 
            "2020-03-29"
        let newElement =
            new XElement(XName.Get element.Name.LocalName,
                new XAttribute(XName.Get element.FirstAttribute.Name.LocalName,
                               newValue),
                    element.Elements())
        newElement
    let changeInformation ( element : XElement ) =
        let newName = 
            element.FirstAttribute.Value
            |> fun x -> x.Replace(";D",";E")
        let newElement =
            new XElement(XName.Get element.Name.LocalName,
                new XAttribute(XName.Get element.FirstAttribute.Name.LocalName,
                               newName),
                    element.Elements())
        newElement

    fatherElement.Elements()
    |> Seq.map (fun xEl ->
        let diggingElement =
            names
            |> Seq.tryFind (fun name ->
                xEl.Name.LocalName = name)
        xEl
        |> function
            | x when x.Name.LocalName.Trim() = "Products" ->
                        let newBottomElements =
                            newValues
                            |> Seq.map (fun prod ->
                                new XElement(XName.Get "Product",
                                    new XAttribute(XName.Get "ProductNumber",
                                               prod.Prod),
                                    new XAttribute(XName.Get "RState",
                                                        prod.Rstate)))
                            
                        let newEl =
                            new XElement(XName.Get x.Name.LocalName,
                                x.Attributes(),
                                newBottomElements)
                        newEl
            | x when x.Name.LocalName = "CriteriaReferenceWithRevision" ->
                x |> changeInformation
            | x when x.Name.LocalName = "InputDateWithIndex" ->
                x |> changeDate
            | x when diggingElement.IsSome ->
                xmlDiging x names newValues
                
            | _ -> xEl)
    |> fun newChildren ->
        let newEl =
            new XElement(XName.Get fatherElement.Name.LocalName,
                                        fatherElement.Attributes(),
                                        newChildren)
        newEl
let getTranslations foundGroup =
    let result =
        foundGroup
        |> Seq.map (fun abbreviation ->
            translations
            |> Seq.tryFind (fun translation ->
                translation.Abbreviation = abbreviation))
        |> Seq.choose id
        |> Seq.collect (fun translation ->
            translation.Products)
    result

let groupIsFound ( key : XElement )
                 ( options : seq<seq<string>> ) =
    options
    |> Seq.tryFind (fun excelProdGroup ->
        key.XPathSelectElements("*//Product")
        |> Seq.exists (fun prodEl ->
            excelProdGroup
            |> Seq.exists (fun excelProdInGroup ->
                let prodElMod = 
                    prodEl.FirstAttribute.Value.Replace(" ","")
                prodElMod.Contains(excelProdInGroup.Replace("*","")))))
let allGroupsFound ( key : XElement )
                   ( options : seq<seq<string>> ) =
    options
    |> Seq.forall (fun excelProdGroup ->
        key.XPathSelectElements("*//Product")
        |> Seq.exists (fun prodEl ->
            excelProdGroup
            |> Seq.exists (fun excelProdInGroup ->
                let prodElMod = 
                    prodEl.FirstAttribute.Value.Replace(" ","")
                prodElMod.Contains(excelProdInGroup.Replace("*","")))))
    |> function
        | res when res = true ->
            options
            |> Seq.collect( fun sequence -> sequence)
            |> Seq.distinct
            |> Some
        | _ -> None

let newSearchKeys =
    relevantCriteriasAsElements
    |> Seq.map (fun key ->
        match sequenceOfCriteriaTypeOption with
        | Some sequenceOfCriteriaTyp ->
            sequenceOfCriteriaTyp
            |> Seq.tryFind (fun excelInfo ->
                key.FirstAttribute.Value.Contains(excelInfo.Name))
            |> function
                | res when res.IsSome ->
                        match res.Value.Products with 
                        | Single ->
                            let currProds =
                                key.XPathSelectElements("*//Product")
                                |> Seq.map (fun el ->
                                    {| 
                                        Prod = el.FirstAttribute.Value
                                        Rstate = "*"
                                    |})
                                |> Seq.append newProdsSequence
                            xmlDiging key xmlLevels currProds 
                        | Divided options ->
                            let oneGroupIsFound =
                                options
                                |> groupIsFound key
                            let allGroupsWereFound =
                                options
                                |> allGroupsFound key
                            ()
                            |> function 
                                | _ when allGroupsWereFound.IsSome ->
                                    allGroupsWereFound.Value
                                    |> getTranslations
                                    |> xmlDiging key xmlLevels
                                | _ when oneGroupIsFound.IsSome ->
                                    oneGroupIsFound.Value
                                    |> getTranslations
                                    |> xmlDiging key xmlLevels
                                | _ -> key
                | _ -> key
        | _ -> key)


let rec replaceXmlElement ( fatherElement : XElement )
                            names
                          ( newElements : seq<XElement> )  =

    let findEl ( elements : seq<XElement> )
               ( currElement : XElement ) =
        elements
        |> Seq.tryFind (fun el ->
            ()
            |> function 
                | _ when currElement.HasAttributes = false ->
                    false
                | _ -> 
                    el.FirstAttribute.Value = currElement.FirstAttribute.Value)
            |> function
                | res when res.IsSome ->
                    let newName = 
                        res.Value.FirstAttribute.Value
                        |> fun x -> x.Replace(";D",";E")
                    let newElement =
                        new XElement(XName.Get res.Value.Name.LocalName,
                            new XAttribute(XName.Get res.Value.FirstAttribute.Name.LocalName,
                                           newName),
                                res.Value.Elements())
                    newElement
                    |> Some
                | _ -> None
        |> function
            | res when res.IsSome ->
                res
            | _ -> None

    fatherElement.Elements()
    |> Seq.map (fun xEl ->
        let newName =
            names
            |> Seq.tryFind (fun name ->
                name = xEl.Name.LocalName)

        let elementOfInterest =
            xEl
            |> findEl newElements
        xEl
        |> function
            | _ when elementOfInterest.IsSome ->
                        elementOfInterest.Value
            | x when newName.IsSome ->
                replaceXmlElement x names newElements
            | _ -> xEl)
    |> fun newChildren ->
        let newEl =
            new XElement(XName.Get fatherElement.Name.LocalName,
                                        fatherElement.Attributes(),
                                        newChildren)
        newEl


let hwlogCriteriaLevels =
    seq[
        "SearchGroups"
        "SearchGroup"
        "SearchKeys"
    ]
newSearchKeys
|> replaceXmlElement xmlFile.Root hwlogCriteriaLevels
|> fun str ->
    File.WriteAllText("C:\Users\egoljos\Documents\ScriptDocuments\1_37_Criteria.txt",str.ToString())
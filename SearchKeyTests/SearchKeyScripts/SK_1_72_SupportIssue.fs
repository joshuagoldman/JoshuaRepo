module SK_1_72_SupportIssue


open System.IO
open System.Xml.Linq
open System.Xml.XPath
open System
open System.Text.RegularExpressions
open System.Linq

let HwLogCriteriaStream = File.Open("C:\Users\egoljos\Documents\Gitrepos\LogAnalyzer\Ericsson.AM.LogAnalyzer\EmbeddedCriteria\RBS6000\Aftermarket\HWLogCriteria.xml", FileMode.OpenOrCreate)
let mutable xDoc = XDocument.Load(HwLogCriteriaStream)

type Attributes = {
    Name : string
    Value : string
}

type ElementInfo = {
    Name : string
    Attributes : Attributes
    NewAttributes : seq<XAttribute>
}

let newELement ( element : XElement)
                ( elInfo : ElementInfo ) =

    let mutable eleMutable =
        element
    let xpath =
        String.Format(
            "*//{0}[@{1}='{2}']",
            elInfo.Name,
            elInfo.Attributes.Name,
            elInfo.Attributes.Value
        )

    let newELement =
        new XElement(XName.Get elInfo.Name,
            elInfo.NewAttributes)
    
    eleMutable.XPathSelectElement(xpath).ReplaceWith(newELement)

    eleMutable



let all_1_72_SearchKeys =
    let allSearchKeys =
        xDoc.XPathSelectElements("*//SearchKey")

    let result =
        allSearchKeys
        |> Seq.filter (fun key -> 
            key.FirstAttribute.Value.Contains("Rule for ERS BB units with SMEM/CMEM ECC bit errors"))

    result
   
let newELements = 
    
    let xpath =
        String.Format(
            "*//{0}[@{1}='{2}']",
            "Variable",
            "Name",
            "X"
        )

    let newVarInfo =
        all_1_72_SearchKeys
        |> Seq.map (fun el ->
            let variableEL = 
                el.XPathSelectElement(xpath)

            let oldVarValue =
                variableEL.Attributes().ElementAt(1).Value

            let newVarValue =
                oldVarValue.Replace(".*0x", ".*synd:0x")

            newVarValue
            )
        |> Seq.map (fun attrName ->
            seq[
                new XAttribute(XName.Get "Name", "X")
                new XAttribute(XName.Get "Value", attrName)
                new XAttribute(XName.Get "IsRegex", "TRUE")

            ])
        |> Seq.map (fun attributes ->
            {
                Name = "Variable"

                Attributes = {
                    Name = "Name"
                    Value = "X"
                }

                NewAttributes = attributes
            })

    //let newDateInfo =
    //    newVarInfo
    //    |> Seq.map (fun _ ->
    //        {
    //            Name = "Variable"

    //            Attributes = {
    //                Name = "Name"
    //                Value = "X"
    //            }

    //            NewAttributes = attributes
    //        }
    //        )

    

    Seq.zip all_1_72_SearchKeys newVarInfo
    |> Seq.map (fun (newInfo,el) ->
        let result =
            el
            |> newELement newInfo
        result)

newELements
|> Seq.map (fun el -> 
    el.ToString() + "\n")
|> String.concat ""
|> fun finalString ->
    File.WriteAllText("C:\Users\egoljos\Documents\ScriptDocuments\Test.txt",finalString)


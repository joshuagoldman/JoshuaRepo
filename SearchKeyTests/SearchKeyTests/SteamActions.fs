module SteamActions


open System
open Xunit
open Ericsson.AM.Common.Definitions
open Ericsson.AM.Common.Logging
open Ericsson.AM.LogAnalyzer.BLL
open Ericsson.AM.LogAnalyzer.Definitions
open Ericsson.AM.LogAnalyzer.Models
open System
open System.Collections.Generic
open System.Data
open System.IO
open System.Linq
open System.Xml
open Ericsson.AM.LogAnalyzer
open Xunit.Abstractions
open System.Text.RegularExpressions
open System.Xml.Linq
open System.Xml.XPath

type LogExistance<'t> =
| Exist of 't
| NotExist

type LogSearchResult =
| Hit of string
| NoHit

type SteamNames = {
    RefTestCases : seq<string>
    NewTemplateName : string
    Tag : string
    ProductInfo : seq<WriteNewProducts2Add.PRTTTestCaseInfo>
}

type ItpTestFile = {
    TestFileID : string
    FileTypeID : string
    LayoutFileID : string
    ProductNo : string
    RState : string
    Description : string
    Position : string
    EntryPoint : string
    Tag : string
}

type ItpTestcase = 
    {
        TestCaseNumber : string
        TestCaseID : string
        Name : string
        Description : string
        ProductNo : string
        R_State : string
        TestType : string
        CategoryNo :string
        TestFiles : seq<ItpTestFile>
    }

let getProperty ( propName : string ) input =
    let regexStr =
        String.Format(
            "(?<=<{0}>).*?(?=<\/{0}>)",
            propName
        )
    let regexToWorkWith =
        new Regex(regexStr)

    let result =
        regexToWorkWith.Match(input).Value

    result

[<Fact>]
[<Trait("Category","Steam Actions")>]
let ``CreateTemplate`` () =

    let productsInfoLAT =
        WriteNewProducts2Add.getAllProdInfo 
            "C:\Users\egoljos\Documents\ScriptDocuments\TemplateInfoLAT.xml"
            "R10L"
    let productsInfoPRTT = 
        WriteNewProducts2Add.getAllProdInfo 
            "C:\Users\egoljos\Documents\ScriptDocuments\TemplateInfoPRTT.xml"
            "R10L"

    let SteamNamesSeq =
        seq[
 //           {
 //               RefTestCases = seq["REF 000 000/1:R10L:LAT:"]
 //               NewTemplateName = "LAT_R10L01_Lab"
 //               Tag = "1/LPA_R10L01"
 //               ProductInfo = productsInfoLAT
  //          }

            {
                RefTestCases = ["REF 000 000/1:R10L:PRTT:";"REF 000 000/1:R10L:HC:";"LPA 107 672/10:R3A:CalRBS6000PRTx64:"]
                NewTemplateName = "Prtt_R10L01_Lab"
                Tag = "1/LPA_R10L01"
                ProductInfo = productsInfoPRTT
            }
            
        ]

    let xmlInfoFileContent =
        File.ReadAllText("C:\Users\egoljos\Documents\SteamTemplates\TemplateInfo.xml")

    let dir = new DirectoryInfo("C:\Users\egoljos\Documents\SteamTemplates\TestFiles")

    let testCaseFilesWithInfo ( testCaseNumber : string ) =
        let testCaseTestFilesStringChunksRegex =
            let regexStr =
                String.Format(
                    "(?<=FileForTestCase_{0})(\n|.)*?(?=<\/FileForTestCase_{0})",
                    testCaseNumber
                )
            new Regex(regexStr)

        let testFilesMatches =
            testCaseTestFilesStringChunksRegex.Matches(xmlInfoFileContent)

        let result =
            [0..testFilesMatches.Count - 1]
            |> Seq.map (fun pos ->
                let testFileStrChunk =
                    testFilesMatches.[pos].Value
                {
                    TestFileID = getProperty "TestFileID" testFileStrChunk
                    FileTypeID = getProperty "FileTypeID" testFileStrChunk
                    LayoutFileID = getProperty "LayoutFileID" testFileStrChunk
                    ProductNo = getProperty "ProductNo" testFileStrChunk
                    RState = getProperty "R-State" testFileStrChunk
                    Description = getProperty "Description" testFileStrChunk
                    Position = getProperty "Position" testFileStrChunk
                    EntryPoint = getProperty "EntryPoint" testFileStrChunk
                    Tag = getProperty "Tag" testFileStrChunk
                })
        result
    
    let steamFiles =
        dir.GetFiles()

    let allTestCasesInfo =
        let testCaseRegex =
            new Regex("(?<=<TestCase)(\n|.)*?(?=<\/TestCase_)")

        let testCaseMatches =
            testCaseRegex.Matches(xmlInfoFileContent)

        let result =
            [0..testCaseMatches.Count - 1]
            |> Seq.map (fun pos ->
                let tcInfo =
                    testCaseMatches.[pos].Value
                
                let testCaseNumber =
                    Regex.Match(tcInfo,"(?<=_)[0-9]{1,2}(?=>)").Value

                {
                    TestCaseNumber = testCaseNumber
                    TestCaseID = getProperty "TestCaseID" tcInfo
                    Name = getProperty "Name" tcInfo
                    Description = getProperty "Description" tcInfo
                    ProductNo = getProperty "ProductNo" tcInfo
                    R_State = getProperty "R-State" tcInfo
                    TestType = getProperty "TestType" tcInfo
                    CategoryNo = getProperty "CategoryNo" tcInfo
                    TestFiles = testCaseNumber |> testCaseFilesWithInfo
                })

        result
    
    let newXmlFileInfo ( testcases : seq<ItpTestcase> )
                       ( templateInfo : SteamNames ) =

        let templateXmlElement =
            new XElement(XName.Get "Template",
                seq[
                    new XElement(XName.Get "Name", templateInfo.NewTemplateName)
                    new XElement(XName.Get "Description")
                    new XElement(XName.Get "Tag", templateInfo.Tag)
                ])

        let noOfTestcasesXml =
            let filtereProducts =
                templateInfo.ProductInfo
                |> Seq.filter (fun prod ->
                    testcases
                    |> Seq.exists (fun tc ->
                        tc.Description = prod.Description))
            new XElement(XName.Get "NumberOfTestCases", 
                new XElement(XName.Get "Quantity", filtereProducts |> Seq.length))

        let tcHeader tcNumber =
            String.Format(
                "TestCase_{0}",
                (tcNumber : string)
            )

        let tfHeader tcNumber =
            String.Format(
                "FileForTestCase_{0}",
                (tcNumber : string)
            )
        let testCaseXml (tcInfo : WriteNewProducts2Add.PRTTTestCaseInfo) tcNumber =
            new XElement(XName.Get (tcHeader tcNumber), 
                seq[
                    new XElement(XName.Get "TestFileID", tcInfo.TestCaseID)
                    new XElement(XName.Get "Name", tcInfo.Name)
                    new XElement(XName.Get "Description", tcInfo.TestType)
                    new XElement(XName.Get "ProductNo", tcInfo.Description)
                    new XElement(XName.Get "R-State", tcInfo.Revision)
                    new XElement(XName.Get "TestType", tcInfo.TestType)
                    new XElement(XName.Get "CategoryNo", tcInfo.CategoryNumber)
                    new XElement(XName.Get "ConfigFilter", "")
                ]
                )

        let testFilesForTestCasesXml testCase tcNumber =
            testCase.TestFiles
            |> Seq.map (fun tf ->
                new XElement(XName.Get(tfHeader tcNumber), 
                    seq[
                        new XElement(XName.Get "TestFileID", tf.TestFileID)
                        new XElement(XName.Get "FileTypeID", tf.FileTypeID)
                        new XElement(XName.Get "LayoutFileID", tf.LayoutFileID)
                        new XElement(XName.Get "ProductNo", tf.ProductNo)
                        new XElement(XName.Get "R-State", tf.RState)
                        new XElement(XName.Get "Description", tf.Description)
                        new XElement(XName.Get "Position", tf.Position)
                        new XElement(XName.Get "EntryPoint", tf.EntryPoint)
                        new XElement(XName.Get "Tag", tf.Tag)
                    ]
                ))
            

        let xmlBody =
            testcases
            |> Seq.collect (fun testCase ->
                templateInfo.ProductInfo
                |> Seq.indexed
                |> Seq.filter (fun (_,info )->
                    info.Description = testCase.Description)
                |> Seq.collect (fun (pos,prodInfo) ->
                    let tcNumber =
                        (pos + 1) |> string
                    let tcXml = 
                        testCaseXml prodInfo tcNumber
                    let tfXmls =
                        testFilesForTestCasesXml testCase tcNumber
                    tfXmls
                    |> Seq.append [tcXml]))

//        let len = xmlBody |> Seq.length
//
//        let xmlBodyAsString =
//            xmlBody 
//            |> Seq.map(fun el ->
//                el.ToString() + "\n")
//            |> String.concat ""
//            |> fun str -> str.Substring(0,str.LastIndexOf("\n"))


        let xmlTree =
            new XElement(XName.Get "Steam_Export", 
                xmlBody
                |> Seq.sortBy(fun c -> 
                    Console.WriteLine(c.Name.LocalName)
                    Regex.Match(c.Name.LocalName,"[0-9]{1,4}").Value
                    |> float)
                |> Seq.append(
                    seq[
                        new XElement(XName.Get "Version_Info",
                            new XElement(XName.Get "Version", "1.00"))
                            
                        templateXmlElement
                        noOfTestcasesXml
                    ]
                )
            )

        xmlTree

    let actions =
        SteamNamesSeq
        |> Seq.iter (fun steamNames ->
            allTestCasesInfo
            |> Seq.filter (fun tc ->
                steamNames.RefTestCases
                |> Seq.exists (fun rfTc -> tc.Name = rfTc) )
            |> function
                | tcInfos when tcInfos |> Seq.length <> 0 ->

                    let tcInfoTestFilesRegex =
                        let regStr =
                            tcInfos
                            |> Seq.collect (fun tcInfo ->
                                tcInfo.TestFiles
                                |> Seq.map (fun tf ->
                                    tf.TestFileID + "|"))
                            |> String.concat ""
                            |> fun str -> str.Substring(0,str.LastIndexOf("|"))
                            |> fun str ->
                                String.Format(
                                    "(?<=File_)({0})(?=\.steam)",
                                    str
                                )
                        new Regex(regStr)

                    let filesToExport =
                        steamFiles
                        |> Seq.filter (fun file ->
                                tcInfoTestFilesRegex.IsMatch(file.Name))

                    ()
                    |> function
                        | _ when filesToExport |> Seq.length <> 0 ->
                                let directory =
                                    Directory.CreateDirectory(Directory.GetCurrentDirectory() + "\\" + steamNames.NewTemplateName)

                                filesToExport
                                |> Seq.iter (fun file ->
                                    ()
                                    |>function
                                        | _ when File.Exists(directory.Name + "\\" + file.Name) = false ->
                                            file.CopyTo(directory.Name + "\\" + file.Name)
                                            |> fun _ -> ()
                                        | _ -> ())

                                let xmlFinal =
                                    newXmlFileInfo
                                        tcInfos
                                        steamNames
                                    |> string

                                XDocument.Parse(xmlFinal).Save(directory.Name + "\TemplateInfo.xml")
                                
                        | _ ->
                            Console.WriteLine("Couldn't find any matching steam test files")
                | _ -> 
                    let errorMsg =
                        String.Format(
                            "Couldn't find info for ref test cases {0}",
                            steamNames.RefTestCases
                            |> Seq.map ( fun tc -> tc + ", ")
                            |> String.concat ""
                            |> fun str  -> str.Substring(0,str.LastIndexOf(", ")) + "."
                        )
                    Console.WriteLine(errorMsg))

    actions
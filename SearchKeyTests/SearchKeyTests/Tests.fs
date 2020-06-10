module Tests

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

type LogExistance<'t> =
| Exist of 't
| NotExist

type LogSearchResult =
| Hit of string
| NoHit

type logTypes = 
    {
        Id : int
        ValidRows : LogExistance<string * int>
        CrrDateRow : LogExistance<string>
        InValidRows : LogExistance<string>
        X1 : LogExistance<string>
        X2 : LogExistance<string>
        Result :LogSearchResult
    }

[<Fact>]
[<Trait("Category","Search Key Scripting")>]
let ``SearchKey_2_43`` () =
    let currentProduct = 
        new ProductModel
            (
                ProductNumber = "KRC*",
                SerialNumber = "*",
                RState = "*"
            )
    

    let mutable analyzer = 
        new LogAnalyzer
            (
                new LogAnalyzerConfiguration
                    (
                        LogSearchGroup = "Radio",
                        LogCriteriaFile = null,
                        Product = currentProduct,
                        TestType = StationTestType.All,
                        ServiceLocation = "SE-SYNGA",
                        EmbeddedCriteria = EmbeddedCriteriaType.RBS6000_AFTERMARKET,
                        PqatClient = null
                    )
            )



    analyzer.AnalyzeLogs (new DirectoryInfo("TestLogFiles/"), currentProduct)
    |> ignore


    let allLogs = 
        {|
            ValidRow = "[190830 230026]"
            CurrDateRow = "[" + DateTime.Now.ToString("yyMMdd hhmmss") + "]"
            InvalidRows = ("[000101", "[700101", "[900101")
            X1 = "2: Event log cleared by command"
            X2 = "8: COLI command: elog clear"
        |}
    
    let allCases =
        seq
         [
            { 
                Id = 1
                ValidRows = Exist(allLogs.ValidRow, 1)
                CrrDateRow = NotExist
                InValidRows = NotExist
                X1  = NotExist
                X2 = NotExist
                Result = NoHit

            }

            { 
                Id = 2
                ValidRows = NotExist
                CrrDateRow = NotExist
                InValidRows = NotExist
                X1  = NotExist
                X2 = NotExist
                Result = Hit "Unused_LAT_2/-43_Rev_A"

            }

            { 
                Id = 3
                ValidRows = NotExist
                CrrDateRow = Exist allLogs.CurrDateRow
                InValidRows = NotExist
                X1  = NotExist
                X2 = NotExist
                Result = Hit "Unused_LAT_2/-43_Rev_A"

            }

            { 
                Id = 4
                ValidRows = NotExist
                CrrDateRow = Exist allLogs.CurrDateRow
                InValidRows = Exist (allLogs.InvalidRows |> fun (log1,log2,_) -> log1 + "\n" + log2)
                X1  = NotExist
                X2 = NotExist
                Result = Hit "Unused_LAT_2/-43_Rev_A"

            }

            { 
                Id = 5
                ValidRows = NotExist
                CrrDateRow = Exist allLogs.CurrDateRow
                InValidRows = Exist (allLogs.InvalidRows |> fun (_,log2,log3) -> log3 + "\n" + log2)
                X1  = Exist allLogs.X1
                X2 = Exist allLogs.X2
                Result = Hit "Unused_LAT_2/-43_Rev_A"

            }

            { 
                Id = 6
                ValidRows = Exist(allLogs.ValidRow, 1)
                CrrDateRow = NotExist
                InValidRows = NotExist
                X1  = Exist allLogs.X1
                X2 = NotExist
                Result = Hit "Elog_erased_LAT_2/-43_Rev_A"

            }

            { 
                Id = 7
                ValidRows = Exist(allLogs.ValidRow, 1)
                CrrDateRow = NotExist
                InValidRows = NotExist
                X1  = NotExist
                X2 = Exist allLogs.X2
                Result = Hit "Elog_erased_LAT_2/-43_Rev_A"

            }

            { 
                Id = 8
                ValidRows = Exist(allLogs.ValidRow, 2)
                CrrDateRow = Exist allLogs.CurrDateRow
                InValidRows = Exist (allLogs.InvalidRows |> fun (log1,log2,_) -> log1 + "\n" + log2)
                X1  = Exist allLogs.X1
                X2 = Exist allLogs.X2
                Result = Hit "Elog_erased_LAT_2/-43_Rev_A"

            }

            { 
                Id = 9
                ValidRows = Exist(allLogs.ValidRow, 2)
                CrrDateRow =NotExist
                InValidRows = NotExist
                X1  = Exist allLogs.X1
                X2 = Exist allLogs.X2
                Result = Hit "Elog_erased_LAT_2/-43_Rev_A"

            }

            { 
                Id = 10
                ValidRows = Exist(allLogs.ValidRow, 10)
                CrrDateRow = Exist allLogs.CurrDateRow
                InValidRows = NotExist
                X1  = NotExist
                X2 = Exist allLogs.X2
                Result = Hit "Elog_erased_LAT_2/-43_Rev_A"

            }

            { 
                Id = 11
                ValidRows = Exist(allLogs.ValidRow, 11)
                CrrDateRow = NotExist
                InValidRows = NotExist
                X1  = Exist allLogs.X1
                X2 = NotExist
                Result = NoHit

            }

            { 
                Id = 12
                ValidRows = Exist(allLogs.ValidRow, 10)
                CrrDateRow = Exist allLogs.CurrDateRow
                InValidRows = Exist (allLogs.InvalidRows |> fun (log1,log2,_) -> log1 + "\n" + log2)
                X1  = Exist allLogs.X1
                X2 = NotExist
                Result = Hit "Elog_erased_LAT_2/-43_Rev_A"

            }
         ]

    let logInsert log =
        File.WriteAllText("TestLogFiles/elogread.txt", log )
        |> ignore

    let matchAndGet (logExistance : LogExistance<string>) =
        match logExistance with
        | Exist (log : string ) -> log + "\n"
        | NotExist -> ""

    let matchAndGetTuple (logExistance : LogExistance<string * int>) =
        match logExistance with
        | Exist (msg : string * int) -> msg
                                        |> fun (log,repeat) ->
                                                seq[0..repeat - 1]
                                                |> Seq.map (fun _ -> log + "\n")
                                                |> String.concat "sssss"
                                                |> fun x -> x.Replace("sssss", "") 
        | NotExist -> ""

    let matchResult result =
        match result with
        | Hit key -> key
        | NoHit -> ""
    
    let performLogSearch logComplete result =
        logInsert logComplete
        |> ignore

        analyzer.AnalyzeLogs (new DirectoryInfo("TestLogFiles/"), currentProduct)
        |> ignore

        analyzer.LogSearchResultModel.LogSearchKeysHits
        |> Seq.tryFind (fun hit -> hit.Name = (matchResult result) )
        |> function
           | res when res = None ->
                        match result with
                        | Hit _ -> false
                        | NoHit -> true
           | _ ->
               match result with
               | Hit _ -> true
               | NoHit -> false

        |> Assert.True 



    
    allCases
    |> Seq.map (fun case -> {| 
                                CompleteLog = 
                                            matchAndGet (case.CrrDateRow) +
                                            matchAndGet (case.InValidRows) +
                                            matchAndGet (case.X1) +
                                            matchAndGet (case.X2) +
                                            matchAndGetTuple (case.ValidRows) +
                                            ("\n" + (case.Id |> string))
                                Result = case.Result
                                Id = case.Id
                             |}  )
    |> Seq.iter (fun case ->
            performLogSearch case.CompleteLog case.Result)

[<Fact>]
[<Trait("Category","Rco Tests")>]
let ``IsRcoListValid`` () =
    let result =
        RcoListFault.matches

    result = 1
    |> Xunit.Assert.True

[<Fact>]
[<Trait("Category","Search Key Scripting")>]
let ``CreateTableData_SK_1_15`` () =
    let result =
        File.ReadAllText("C:\Users\egoljos\Documents\ScriptDocuments\SK_1_15_Tables.txt")

    let unitTableRegex =
        new Regex("DUW(\n|.)*?Table")

    let componentWSNIDRegex = 
        new Regex("(?<![0-9])D[0-9]{1,5}.*")

    let SNIDNumberRegex =
        new Regex("(?<=\s)[0-9]{2,5}")

    let componentRegex =
        new Regex("D.*?(?=\s)")

    let tables =
        let allInfoMatches =
            unitTableRegex.Matches(result)

        [0..allInfoMatches.Count - 1]
        |> Seq.map (fun pos -> 
            allInfoMatches.[pos].Value)

    let nyquistWithSNIDS table =
        let allInfoMatches =
            componentWSNIDRegex.Matches(table)

        [0..allInfoMatches.Count - 1]
        |> Seq.map (fun pos -> 
            allInfoMatches.[pos].Value)
        |> Seq.map (fun info ->

            let ``component`` =
                componentRegex.Match(info).Value

            let SNIDNumMatches =
                SNIDNumberRegex.Matches(info)

            let SNIDNumbers =
                [0..SNIDNumMatches.Count - 1]
                |> Seq.map (fun pos ->
                    SNIDNumMatches.[pos].Value)

            {|
                Component = ``component``
                SNIDs = SNIDNumbers
            |})
    
    let allTablesAsobject =
        tables
        |> Seq.map (fun table ->
            table
            |> nyquistWithSNIDS)
    let finalString =
        allTablesAsobject
        |> Seq.map (fun nyquists ->
            nyquists
            |> Seq.map (fun nyquist ->

                let snids =
                    let snidsInit =
                        nyquist.SNIDs
                        |> Seq.map (fun snid ->
                            snid + ":")
                        |> String.concat ""
                        |> fun str -> str.Substring(0, str.LastIndexOf(":"))

                    let noOfSnids =
                        Regex.Matches(snidsInit,":").Count

                    let noOfColons2Add =
                        3-noOfSnids

                    let colons2Add =
                        [0..noOfColons2Add]
                        |> Seq.map (fun _ -> ":")
                        |> String.concat ""
                        |> fun str -> str.Substring(0, str.LastIndexOf(":"))

                    snidsInit + colons2Add

                let totString = nyquist.Component + ":" + snids + ";"

                totString)
            |> String.concat ""
                )
        |> String.concat "\n\n"
        |> fun str -> str.Substring(0, str.LastIndexOf(";"))


    File.WriteAllText("C:\Users\egoljos\Documents\ScriptDocuments\SNIDSTableInfo.txt",finalString)

type RstateOptions =
    | All
    | Specific of string

type RcoInformation = {
    Rco : string
    ProductNumber : string
    RstateIn : RstateOptions
    RstateOut : RstateOptions
    RcoRev : string
}

type ProductWithRevision = {
    Rstate : string
    ProductNumber : string
}

type Comparison = 
    | IsLargerThan
    | IsEqual
    | IsLessThan

[<Fact>]
[<Trait("Category","Rco Tests")>]
let ``RCO_Search`` () = 
    let getMatches (col : MatchCollection) =
        [0..col.Count - 1]
        |> Seq.map (fun pos ->
            col.[pos].Value)
    let rcoFileContent =
        File.ReadAllText("C:\Users\egoljos\Documents\Gitrepos\LogAnalyzer\Ericsson.AM.RcoHandler\EmbeddedResources\RBS6000\Aftermarket\RBS RCO List.csv")

    let rstateMatchRegex =
        new Regex("All|R[0-9][A-Z](\/[A-Z]|)")

    let prodNumberWithRsatesRegex prodNumber =
        new Regex("(?<=\&\@\?)" + prodNumber + "\&\@\?.*(All\&\@\?|[A-Z][0-9][A-Z](\/[A-Z]|)(\&\@\?))");

    let relevantRowsRegex prodNumber =
        new Regex(".*(?<=\&\@\?)" + prodNumber + "\&\@\?.*(All|[A-Z][0-9][A-Z](\/[A-Z]|))(\&\@\?)");

    let rcoRegex =
        new Regex("(?<=[0-9]{8}\&\@\?).*?(?=\&\@\?)")

    let rcoRevRegex =
        new Regex("(?<=[0-9]{8}\&\@\?.*?\&\@\?)[A-Z](?=\&\@\?)")
        
    let rStateInRegex =
        new Regex("(?<=\&\@\?)(?!All).*(?=\&\@\?)")

    let rStateOutRegex =
        new Regex("(?<=\&\@\?(All|[A-Z][0-9][A-Z](\/[A-Z]|))\&\@\?).*")

    let getRelevantRows productNumber =
        let rgx =
            productNumber
            |> relevantRowsRegex
        rgx.Matches(rcoFileContent)
        |> getMatches

    let rstateCalc (rgx : Regex ) cntnt =
        rgx.Match(cntnt).Value
        |> rstateMatchRegex.Match
        |> function
            | res when res.Success ->
                res.Value
                |> RstateOptions.Specific
            | _ -> 
                RstateOptions.All

    let allRelevantRcos productNumber =
        let relevantRows =
            productNumber
            |> getRelevantRows

        relevantRows
        |> Seq.map (fun row ->
            let rco = 
                rcoRegex.Match(row).Value
                
            let prodNumWithInfo =
                productNumber
                |> prodNumberWithRsatesRegex
                |> fun rgx ->
                    rgx.Match(row).Value
                    
            let rstateIn =
                prodNumWithInfo
                |> rstateCalc rStateInRegex
                
            let rstateOut =
                prodNumWithInfo
                |> rstateCalc rStateOutRegex

            let rcoRev =
                rcoRevRegex.Match(row).Value
                
            {
                Rco = rco
                ProductNumber = productNumber
                RstateIn = rstateIn
                RstateOut = rstateOut
                RcoRev = rcoRev
            })

    let compareStates rstate1 rstate2 =
        ()
        |> function
            | _ when rstate1 = rstate2 ->
                Comparison.IsEqual
            | _ ->
                seq[rstate1;rstate2]
                |> Seq.sort
                |> function
                    | res when (res |> Seq.item 0 |> fun x -> x = rstate1) ->
                        Comparison.IsLessThan
                    | _ -> 
                        Comparison.IsLargerThan

    let rec searchRco ( rcosSpecific : seq<RcoInformation> ) rstateOut =
        rcosSpecific
        |> Seq.choose (fun rcoComp ->
            let result =
                match rcoComp.RstateIn with
                | RstateOptions.Specific rstateIn ->
                    ()
                    |> function
                        | _ when rstateIn.Contains("+") ->
                            let comparisonRes =
                                match (compareStates rstateOut rstateIn) with
                                | Comparison.IsLessThan ->
                                    None
                                | _ ->
                                    Some(seq[rcoComp])

                            comparisonRes
                            
                        | _ when rstateIn.Contains("-") ->
                            let comparisonRes =
                                match (compareStates rstateOut rstateIn) with
                                | Comparison.IsLargerThan ->
                                   None
                                | _ ->
                                   Some(seq[rcoComp])
                            comparisonRes
                        | _ -> 
                            let isRstateOutRstateInHere =
                                rstateIn = rstateOut
                            ()
                            |> function
                                | _ when isRstateOutRstateInHere = true ->
                                    match rcoComp.RstateOut with
                                    | RstateOptions.Specific rstateOutNew ->
                                        ()
                                        |> function
                                            | _ when rstateIn = rstateOutNew -> 
                                                Some(seq[rcoComp])
                                            | _ ->
                                                let recursiveRes =
                                                    searchRco rcosSpecific rstateOutNew
                                                
                                                match recursiveRes with
                                                | Some res ->
                                                    Seq.append res (seq[rcoComp])
                                                    |> Some
                                                | _ -> Some(seq[rcoComp])

                                    | RstateOptions.All ->
                                        Some(seq[rcoComp])
                                | _ ->
                                    None
                | RstateOptions.All ->
                    Some(seq[rcoComp])
                        
            result)
        |> Seq.collect (fun x -> x)
        |> function
            | foundRcos when foundRcos |> Seq.length <> 0 ->
                Some foundRcos
            | _ -> None

    let getRstate state =
        match state with
        | RstateOptions.Specific s ->
            s
        | _ -> 
            "All"

    let performRcoAndWriteResToFile productnumber rcosOfInterest rstate  =
        rstate
        |> searchRco rcosOfInterest
        |> function
            | rcoSearchResult when rcoSearchResult.IsSome ->
                let header =
                    String.Format(
                        "#### {0}, rev {1}:

| Rco | Product Number | RstateIn | RstateOut |
|-----|----------------|-----------|-----------|",
                        productnumber,
                        rstate
                    )
                let tableItself =   
                    rcoSearchResult.Value
                    |> Seq.map (fun rco ->
                        String.Format(
                            "
|{0}|{1}|{2}|{3}|",
                            rco.Rco + ", " + rco.RcoRev,
                            rco.ProductNumber ,
                            getRstate rco.RstateIn,
                            getRstate rco.RstateOut
                    ))
                    |> String.concat ""
                
                let tableFinal = header + tableItself

                Some tableFinal
            | _ -> 
                None



    let productWithRevisionSequence =
        seq[

            {
                Rstate = "R1F"
                ProductNumber = "KRC161255/1"
            }

            {
                Rstate = "R2C"
                ProductNumber = "KRC118001/1"
            }

            {
                Rstate = "R2C"
                ProductNumber = "KRC161619/1"
            }

            {
                Rstate = "R1E"
                ProductNumber = "KRC161622/1"
            }

            {
                Rstate = "R1E"
                ProductNumber = "KRC161625/1"
            }
        ]

    productWithRevisionSequence
    |> Seq.choose (fun info ->
        let rcosOfInterest = allRelevantRcos info.ProductNumber
         
        let rcoResult =
            performRcoAndWriteResToFile info.ProductNumber rcosOfInterest info.Rstate
            
        ()
        |> function
            | _ when rcoResult.IsSome ->
                Some rcoResult.Value
            | _ -> 
                None)
    |> function
        | allSearchesResult when allSearchesResult |> Seq.length <> 0 ->
            let finalResult =
                allSearchesResult
                |> String.concat "\n\n\n"

            File.WriteAllText("C:\Users\egoljos\Documents\ScriptDocuments\Rco_Result.md",finalResult)
        | _ -> 
            File.WriteAllText("C:\Users\egoljos\Documents\ScriptDocuments\Rco_Result.md","NO RCOS FOUND!")

[<Fact>]
[<Trait("Category","Steam Actions")>]
let ``CreateReleaseAutomationFunctions`` () =
    let sites =
        seq[
            "BangladeshDhaka"
            "BrazilSaoJoseDosCampos"
            "HungaryBudapest"
            "USADallas"
            "ChinaNanjing"
            "JabilMexicoGuadalajara"
            "ChinaGuangzhou"
            "IndonesiaJakarta"
            "IndiaPune"
            "IndiaPune Claim"
            "JapanNarita"
            "MalaysiaKualaLumpur"
            "HollandRijen"
            "EstoniaTallinn"
            "ChinaWuxia"
        ]

    let testTypes =
        seq[
            "PRTT"
            "LAT"
        ]

    let allPosibillities =
        testTypes
        |> Seq.collect (fun tp ->
            sites
            |> Seq.map (fun site ->
                site + "_" + tp))

    let getMethod ( posibility : string ) =
            "public void GetProgress_" + posibility + "(ulong current)
{
    var site = \"" + posibility + "\";

    GetProgress(current, site);
}"

    let dictItem( posibility : string ) =
            "{\"" + posibility + "\" , new Action<ulong>(GetProgress_" + posibility + ") }"
        

    let finalString =
        allPosibillities
        |> Seq.map (fun posibility ->
            posibility |> getMethod)
        |> String.concat "
"


    let finalDictionaryString =
        allPosibillities
        |> Seq.map (fun posibility ->
            posibility |> dictItem)
        |> String.concat ",
"

    File.WriteAllText("C:\Users\egoljos\Documents\ScriptDocuments\ReleaseAutomationProgressMethods.txt",finalString + "\n\n\n" + finalDictionaryString)

[<Fact>]
[<Trait("Category","Steam Actions")>]
let ``Copy LAT Release`` () =
    let sites =
        seq[
            "BangladeshDhaka"
            "BrazilSaoJoseDosCampos"
            "HungaryBudapest"
            "USADallas"
            "ChinaNanjing"
            "JabilMexicoGuadalajara"
            "ChinaGuangzhou"
            "IndonesiaJakarta"
            "IndiaPune"
            "IndiaPune Claim"
            "JapanNarita"
            "MalaysiaKualaLumpur"
            "HollandRijen"
            "EstoniaTallinn"
            "ChinaWuxia"
        ]   
        
    sites
    |> Seq.iter (fun site ->
        let siteFileName =
            "C:/Users/egoljos/Documents/Gitrepos/sharedtools/Release Automation/StoreInSteam/StoreInSteamGUI/bin/Debug/Export/LAT_R10M/LAT_R10M_Rel-" + site + ".zip"
        let fileName =
            "C:/Users/egoljos/Documents/Gitrepos/sharedtools/Release Automation/StoreInSteam/StoreInSteamGUI/bin/Debug/Export/LAT_R10M_Rel.zip"
          
        File.Copy(fileName,siteFileName)
            )



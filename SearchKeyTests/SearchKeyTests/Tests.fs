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
let ``IsRcoListValid`` () =
    let result =
        RcoListFault.matches

    result = 1
    |> Xunit.Assert.True
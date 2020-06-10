module UpdateRCOScript

open Microsoft.Office.Core
open System.IO
open System.Diagnostics
open Microsoft.Win32
open ExcelDataReader
open ExcelDataReader.Core
open System.Text




let getxlColumns (filePath : string) =
    System.Text.Encoding.RegisterProvider(System.Text.CodePagesEncodingProvider.Instance)
    let  stream = File.Open(filePath,  FileMode.Open, FileAccess.ReadWrite)
    let  dataReader = ExcelReaderFactory.CreateOpenXmlReader(stream)
    let dataSet = dataReader.AsDataSet()
    let table = dataSet.Tables.[0]

    let mutable result = Seq.empty :> seq<seq<string>>


    [0..table.Columns.Count - 1]
    |> Seq.iter (fun i -> 
                    let mutable temp = Seq.empty :> seq<string>
                    [0..table.Rows.Count - 1]
                    |> Seq.iter (fun j -> 
                                let kaka = table.Rows.[j].[i].ToString()

                                temp <- Seq.append temp [ kaka ])
                    result <- Seq.append result [temp] )

    let mutable allColumnsTranspose = Array.empty :> string [] []

    let allColumns =
        [0..table.Columns.Count - 1]
        |> Seq.map (fun pos -> result |> Seq.item(pos))


   

    let colLength = 
        allColumns
        |> Seq.item 0
        |> fun i -> i |> Seq.length |> fun x -> x - 1

    [|0..colLength|]
    |> Array.iter (fun _ -> 
                            let temp = [|""|]
                            allColumnsTranspose  <- Array.append allColumnsTranspose [|temp|] )
    
    allColumns
    |> Seq.iter (fun col -> Seq.zip col [0..colLength]
                            |> Seq.iter (fun (rowVal,pos) -> 
                                                    let temp =      
                                                        Array.append allColumnsTranspose.[pos] [|rowVal|] 
                                                    allColumnsTranspose.[pos] <- temp ))
       
    let oneSkipped =
        allColumnsTranspose
        |> Array.map (fun row -> row
                                 |> Array.skip 1)

    allColumnsTranspose <-  oneSkipped

    //let colsToInvestigate =
    //    allColumns 
    //    |> Seq.map (fun col -> Seq.zip col [0..col |> Seq.length |> fun x -> x - 1]
    //                           |> Seq.filter (fun (_,pos) -> [1635..1645]
    //                                                         |> Seq.exists (fun posComp -> posComp = pos)))

                                        
    
    let finalString =
        allColumnsTranspose
        |> Seq.map (fun row -> row
                               |> Seq.map (fun col -> col + "&@?")
                               |> String.concat ""
                               |> fun x -> x.Substring(0, x.LastIndexOf("&@?")))
        |> Seq.map (fun row -> row + "\r\n")
        |> String.concat ""
        |> fun x -> x.Substring(0, x.LastIndexOf("\r\n"))

    File.WriteAllText("C:\Users\egoljos\Documents\Gitrepos\LogAnalyzer\Ericsson.AM.RcoHandler\EmbeddedResources\RBS6000\Aftermarket\RBS RCO List.csv", finalString)
    

getxlColumns "C:\Users\egoljos\Documents\ScriptDocuments\RCO_List_May_4Script.xlsx"
|> ignore




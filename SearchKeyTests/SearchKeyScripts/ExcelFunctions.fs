module ExcelFunctions

open Microsoft.Office.Core
open System.IO
open System.Diagnostics
open Microsoft.Win32
open ExcelDataReader
open ExcelDataReader.Core
open System.Text

type ExcelData<'t> = {
    DataColumnWise : 't
    DataRowWise : 't
}
let getxlColumns (filePath : string) =
    //System.Text.Encoding.RegisterProvider(System.Text.CodePagesEncodingProvider.Instance)
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

    let allColumnsTransposeSequenced =
        allColumnsTranspose
        |> Array.map (fun row -> row |> Array.toSeq)
        |> Array.toSeq

    {
        DataColumnWise = allColumns
        DataRowWise = allColumnsTransposeSequenced
    }

let getxlColumnsDictionary (filePath : string) =
    //System.Text.Encoding.RegisterProvider(System.Text.CodePagesEncodingProvider.Instance)
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

    let allColumnsTransposeSequenced =
        allColumnsTranspose
        |> Array.map (fun row -> row |> Array.toSeq)
        |> Array.toSeq
    
    let createDict data =
        let headerNames =
            data
            |> Seq.item 0
            |> Seq.map (fun (str : string) -> str.Replace("\n",""))
        
        let dictionary =
            data
            |> Seq.skip 1
            |> Seq.map (fun info ->
                Seq.zip headerNames info
                |> dict)

        dictionary

    {
        DataColumnWise = allColumns |> createDict
        DataRowWise = allColumnsTransposeSequenced |> createDict
    }




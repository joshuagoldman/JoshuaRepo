module CriteriaVerificationDefinitions

open System.Text.RegularExpressions
open System
open System.IO

type Product =
    | AllRStates of string
    | RstateFromTo of string * string * string
    | Rstates of string * seq<string>

type Productoptions =
    | Include of seq<Product>
    | Exclude of seq<Product>

type ServiceLocations =
    | Included of seq<string>
    | Excluded of seq<string>

type TestTypes =
    | IncludedType of seq<string>
    | ExcludedType of seq<string>

type VariableType =
    | Log of string * string Option* string
    | Pqat of option<string> * string

type TextOutPut =
    | NoOutPut
    | Output of string

type VerificationSetup = {
    ProductsOptions : Productoptions
    ServiceLocations : ServiceLocations Option
    TestTypes : TestTypes Option
    VariablesSeq : seq<seq<VariableType>> Option
}

type ExpectedResult =
    | Hit of VerificationSetup * seq<TextOutPut>
    | NoHit of VerificationSetup

type SearchKeyCase =
    {
        Name :  string
        Case : ExpectedResult
    }

let write2MarkDown case titleInfo =
    let title = String.Format (
                    "# {0}",
                    case.Name
                )

    let titleWithInfo = String.Format (
                            "{0} ({1})
",
                            title,
                            titleInfo
                        )

    let (setUp,outputs) =
        match case.Case with
        | Hit(setUp,outPuts) ->
            (setUp,Some outPuts)
        | NoHit(setUp) ->
            (setUp,None)

    let productsMessage =
        let (products, baseString) =
            let getProductTxt products =
                products
                |> Seq.map (fun prod ->
                    match prod with
                    | AllRStates prodName ->
                        String.Format(
                            "Product number: {0}, Rstate: Any (*)
    ",
                            prodName
                        )
                    | RstateFromTo(prodName,fromRState,toRstate) ->
                        String.Format(
                            "Product number: {0}, Rstate: from {1} to {2}
    ",
                            prodName,
                            fromRState,
                            toRstate
                        )
                    | Rstates(prodName,rSates) ->
                        String.Format(
                            "productName: {0}, Rstates: {1}
    ",
                            prodName,
                            rSates
                        ))

            match setUp.ProductsOptions with
            | Productoptions.Include (products) ->
                let baseString =
                    "- Use any the following products:
"
                getProductTxt products, baseString
            | Productoptions.Exclude products ->
                let baseString =
                    "- **Do not** use the following products:
"
                getProductTxt products, baseString



        products
        |> String.concat ""
        |> fun str -> str.Substring(0, str.LastIndexOf("\n"))
        |> fun str -> 
            String.Format(
                "{0}
    ```plain
    {1}
    ```
",
                baseString,
                str
            )

    let serviceLocationString =
        match setUp.ServiceLocations with
        | Some locationsAlts ->
            let writeLocations2MarkDown locations baseString =
                locations
                |> Seq.map (fun str -> str + "
    "                                           )
                |> String.concat ""
                |> fun x -> x.Substring(0,x.LastIndexOf("
    "                                                   ))
                |> fun x -> 
                    String.Format(
                        "
    ``` plain
    {0}
    ```
",
                        x
                    )
                |> fun str -> baseString + str 
                
            match locationsAlts with
            | Included locations ->
                let baseString =
                    "- The test station should be set up to be any of the following service locations: "

                writeLocations2MarkDown locations baseString
                
            | Excluded locations ->
                let baseString =
                    "- The test station should **NOT** be set up to any of the following service locations: "

                writeLocations2MarkDown locations baseString

        | None -> "- Test station may be set to any service location.
"


    let testTypesString =
        match setUp.TestTypes with
        | Some locationsAlts ->
            let writeLocations2MarkDown locations baseString =
                locations
                |> Seq.map (fun str -> str + "\n")
                |> String.concat ""
                |> fun x -> x.Substring(0,x.LastIndexOf("\n"))
                |> fun x -> 
                    String.Format(
                        "
    ``` plain
    {0}
    ```
",
                        x
                    )
                |> fun str -> baseString + str 
                
            match locationsAlts with
            | IncludedType types ->
                let baseString =
                    "- The test station should be set up to have any of the following test types: "

                writeLocations2MarkDown types baseString
                
            | ExcludedType types ->
                let baseString =
                    "- The test station should **NOT** be set up to have any of the following test types: "

                writeLocations2MarkDown types baseString

        | None -> "- Test station may be set to any test type.
"
        

    let variablesMessage =
            
        match setUp.VariablesSeq with
        | Some vars ->
            let baseString =
                match  case.Case with
                | Hit _ ->
                
                        "- Verify a hit by following at least **one** of the following steps:
    "
                | NoHit _ ->
                    "- Verify that a hit does **not** occur by following at least **one** of the following alternatives:
    "

            let outputsNew =
                match outputs with
                | Some results ->
                    results
                    |> Seq.map (fun result -> Some result )
                | _ -> 
                    vars
                    |> Seq.map (fun _ -> None)

            let lengths =
                seq[0..vars |> Seq.length |> fun x -> x - 1]

            Seq.zip vars lengths
            |> Seq.map (fun (variables,pos) -> 
                let baseString =
                    String.Format(
                        "- Alternative {0}:
        ",
                        (pos + 1 |> string)
                    )

                let outputString = 
                    match (outputsNew |> Seq.item pos) with
                    | Some op ->
                        match op with
                        | Output str ->
                            String.Format(
                                "- The following text output should be displayed:
        ```plain
        {0}
        ```
        ",
                                str
                            )
                        | _ -> "- No text output will be provided"
                    | _ ->  "- No text output will be provided"
                let variables2Txt =
                    variables
                    |> Seq.map (fun (variable) ->
                        match variable with
                        | Log(log,condition,file) ->
                            condition
                            |> function
                                | condRes when condRes.IsSome ->
                                    String.Format(
                                        "- Insert log **{0}** into file **{1}**. The log should appear {2}.
        ",
                                        log,
                                        file,
                                        condRes.Value
                                    )
                                | _ -> 
                                    String.Format(
                                        "- The log **{0}** should appear 0 times within file(s) **{1}**.
        ",
                                        log,
                                        file
                                    )
                        | Pqat(jsonStr,msg) ->
                            let jSonStrNew =
                                match jsonStr with
                                | Some str ->
                                    String.Format(
                                        "
        ```plain
        {0}
        ```
        ",
                                        str
                                    )
                                    
                                | _ -> "
        "
                                
                            String.Format(
                                    "- {0}{1}",
                                    msg,
                                    jSonStrNew
                                ))
                variables2Txt
                |> String.concat ""
                |> fun varStr ->
                    baseString + varStr + outputString + "
    "
            )
            |> String.concat ""
            |> fun alts ->
                baseString + alts + "
"
        | _ -> "- No text output will be provided
    "
            

    titleWithInfo + productsMessage + serviceLocationString + testTypesString + variablesMessage 
 

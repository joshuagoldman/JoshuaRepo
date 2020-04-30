module Searchkey_2_42

open System.Text.RegularExpressions
open System

let dataStr = 
        """
            KDU137569/* (DUG);	Inactive;	185;	*;	A'
            KDU127161/* (DUWv1);	Inactive;	185;	*;	A'
            KDU137925/* (BB52xx);	MY-ERIKL;	185;	*;	B'
            KDU137925/* (BB52xx);	EE-ERITA;	185;	*;	B'
            KDU127161/3 (DUWv1);	Inactive;	185;	JP;	D'
            KDU137624/1 (DUS41);	Inactive;	185;	JP;	D'
            KDU127189/2 (ODS41);	Inactive;	185;	JP;	D'
            KRC11862/2 (RUS01B8);	Inactive;	185;	JP;	D'
            KRC11875/1 (RUS01B1;	Inactive;	185;	JP;	D'
            KRC161255/2 (RRUS11B1);	Inactive;	185;	JP;	D'
            KRC161262/3 (RRUS12B8);	Inactive;	185;	JP;	D'
            KRC161282/3 (RRUS12B3);	Inactive;	185;	JP;	D'
            KRC161419/1 (RRUS72B41b);	Inactive;	185;	JP;	D'
            KRC161490/2 (Radio 2217);	Inactive;	185;	*;	B'
            KDU127174/* (DUWv2);	Inactive;	185;	;	N/A'
            KDU137624/* (DUS41);	Inactive;	185;	;	N/A'
            KDU137847/* (BB6620);	EE-ERITA;	185;	*;	B'
            KDV127620/* (BB6620);	EE-ERITA;	185;	*;	B'
            KDU137848/* (BB6630);	EE-ERITA;	185;	*;	B'
            KDV127621/* (BB6630);	EE-ERITA;	185;	*;	B
            
        """

type Active =
    | IsActive of string
    | InActive

type InfoMessage =
    | Valid 
    | Invalid

type SearchGroup =
    | Baseband of string
    | DU of string
    | Radio of string
    | UnDefined

type InfoData =
    {
        ProductNumber : string
        SearchGroup : SearchGroup
        Active : Active
        Message : InfoMessage
    }
type SearchKey =
    {
        Locations : string
        searchGroup : string
        Products : string
    }

let getProdNumber strValue =
    let regex = new Regex("((KD|KR)[^\s\(]*)")
    regex.Match(strValue).Value

let getSearchGroup strValue =
    let duRegex = "DUW|DUS|DUG|ODS"
    let radioRegex = "Radio|RRUS|RUS"
    let basebandRegex = "BB"

    strValue
    |> function
       | _ when Regex.Match(strValue, duRegex).Success -> DU "DU"
       | _ when Regex.Match(strValue, radioRegex).Success -> Radio "Radio"
       | _ when Regex.Match(strValue, basebandRegex).Success -> Baseband "Baseband"
       | _ -> UnDefined

let getInfoMessageType msg =
    let regex = new Regex("B")

    match regex.Match(msg).Success with
    | true -> Valid
    | _ -> Invalid

let getActive msg =
    let regex = new Regex("Inactive")
    match regex.Match(msg).Success with
    | true -> InActive
    | false -> IsActive (msg.Trim())  

let data2WorkWith =
    dataStr
    |> fun str -> str.Split '''
                  |> Array.map (fun infoRow -> infoRow.Split ';'
                                                 |> fun row ->
                                                    
                                                        {
                                                            ProductNumber = getProdNumber (row.[0].Trim())
                                                            SearchGroup = getSearchGroup (row.[0].Trim())
                                                            Message = getInfoMessageType (row.[4].Trim())
                                                            Active = getActive (row.[1].Trim())
                                                        })

let result = 
    seq[ Baseband "Baseband" ; DU "DU" ; Radio "Radio" ]
    |> Seq.map (fun sGroup ->
                    data2WorkWith
                    |> Array.filter (fun data -> data.SearchGroup = sGroup &&
                                                 data.Active <> InActive &&
                                                 data.Message = Valid)
                    |> fun x -> 
                            {
                                Locations = (x      
                                             |> Array.map (fun info -> match info.Active with
                                                                       | IsActive location -> "\n" + location
                                                                       | _ -> "")
                                             |> Array.distinct
                                             |> String.concat ","
                                             |> fun str -> str.Replace(",","").Trim())
                                searchGroup = (match sGroup with       
                                                | Baseband str -> str
                                                | DU str -> str
                                                | Radio str -> str
                                                | UnDefined -> "")
                                Products =  (x
                                             |> Array.map ( fun data -> data.ProductNumber + "\n")
                                             |> String.concat "sssss"
                                             |> fun str -> str.Replace("sssss",""))
                            })
    |> Seq.map (fun grp ->  "Search group:\n" + grp.searchGroup +
                            "\nLocations:" + grp.Locations +
                            "\nProducts:" + grp.Products + "\n\n\n")
    |> Seq.iter (fun msg -> Console.Write msg )
    |> fun _ -> Console.ReadLine


result
|> ignore

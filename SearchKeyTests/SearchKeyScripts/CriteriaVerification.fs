namespace CriteriaVerification

open CriteriaVerificationDefinitions
open System.IO
open System

module Template =

    let name = "Generic_2/-40_B"
    let products =
        seq[
            ("KRC 161*" |> AllRStates )
        ]

    let pqatLiefCycleCompChange =
        "{
        \"SerialNo\": \"E23A550430\",
        \"Time\": \"Jan  8 2020 12:00:00:000AM\",
        \"TestPointGroupName\": \"\",
        \"TestPointName\": \"\",
        \"MeasuredValue\": \"\",
        \"Component\": \"\",
        \"ComponentPos\": \"\",
        \"Fault\": \"-\",
        \"RCO\": \"15625-KRC161619/1-4_B\",
        \"Action\": \"A130\"
        }"

    let pqatLatIpAnswerHit =
        "[
            {
                \"Question\": \"E23A550430\",
                \"Answer\": \"15625-KRC161619/1-4\"
            }
        ]"

    let pqatLifeCycleComponentChangeMsgHit =
        ""
    let latIpAnswerMsghit =
        "Use a serial number, for instance E23A550430; E23A186916, such that you get **EXACTLY** one of the following **PQAT LAT IP** answers, see also json eaxmple below:\n\t\t
        ```json\n\t\t
            15625-KRC161619/1-4  \n   
            15625-KRC161650/1-1  \n
            15625-KRC161753/1-1  \n
            15625-KRC161707/2-1_E\n\t\t
            ```"
    
    let variableshit1 =
        seq[
            seq[ 
                (
                    (Some pqatLatIpAnswerHit, latIpAnswerMsghit) |> Pqat
                )  
            ]
        ]

    let variablesNoHit1 =
        seq[
            seq[
                (
                    (Some pqatLatIpAnswerHit, latIpAnswerMsghit) |> Pqat
                )   
            ]
        ]
    
    let output1 = 
        seq[
            (
                ""
                |> Output
            )  
        ]

    let verificationSetups =
        seq[
            (
                {
                    ProductsOptions = products |> Productoptions.Include
                    ServiceLocations = None
                    TestTypes = None
                    VariablesSeq = Some variableshit1
                },
                output1
            )
            |> Hit
        ]
   
    
    let cases =
        verificationSetups
        |> Seq.map (fun case ->
            {
                Name = name
                Case = case
            })

    let filterCases cases isHit =
        cases
        |> Seq.filter (fun case ->
            match case.Case with 
            | Hit _ -> isHit
            | NoHit _ -> not(isHit))
    
    let hitVerInstruction =
        filterCases cases true
        |> fun newCases ->
            Seq.zip newCases [1..newCases |> Seq.length]
        |> Seq.map (fun (case,num) ->
            write2MarkDown case ("hit " + (num |> string))
            )
        |> String.concat ""

    let NohitVerInstruction =
        filterCases cases false
        |> fun newCases ->
            Seq.zip newCases [1..newCases |> Seq.length]
        |> Seq.map (fun (case,num) ->
            write2MarkDown case ("no hit " + (num |> string))
            )
        |> String.concat ""
    
    let finalVerIntructionBoth =
        hitVerInstruction + NohitVerInstruction

module SK_1_63 =

    let name = "EMCA bit-flip issue on BB52, issue1, 1/-63, Rev B"
    let products =
        seq[
            ("KDU 137 925/31"  |> AllRStates )
            ("KDU 137 925/41"  |> AllRStates )
            ("KDV 127 620/11"  |> AllRStates )
            ("KDV 127 620/11"  |> AllRStates )
            ("KDV 127 621/11"  |> AllRStates )
            ("KDU 137 847/11"  |> AllRStates )
            ("KDU 137 848/11"  |> AllRStates )
            ("KDU 137 0053/31" |> AllRStates )
        ]
    
    let variableshit1 =
        seq[
            seq[
                (
                    ("Emca 1:DSP 32: \"Colossus fatal", Some("1 or more times"), "llog") |> Log
                )   
            ]
            seq[
                (
                    ("Emca 2:DSP 32: \"Boot loader code was calle", Some("1 or more times"), "syslog") |> Log
                )
            ]
            seq[
                (
                    ("Emca 4:DSP 32: \"Colossus warning", Some("1 or more times"), "llog") |> Log
                ) 
            ]
        ]

    let variablesNoHit1 =
        seq[
            seq[
                (
                    ("Emca 1:DSP 32: \"Colossus fatal", None, "llog,syslog") |> Log
                ) 
                (
                    ("Emca 2:DSP 32: \"Boot loader code was calle", None, "llog,syslog") |> Log
                )
                (
                    ("Emca 4:DSP 32: \"Colossus warning", None, "llog,syslog") |> Log
                ) 
            ]
        ]
    
    let output1 = 
        seq[
            (
                "HW Fault identified. Replace Trinity component at position D000T1, Report as: A105/59 (Major Fault)"
                |> Output
            )
            (
                "HW Fault identified. Replace Trinity component at position D000T2, Report as: A105/59 (Major Fault)"
                |> Output
            )
            (
                "HW Fault identified. Replace Trinity component at position D000T4, Report as: A105/59 (Major Fault)"
                |> Output
            )       
        ]

    let verificationSetups =
        seq[
            (
                {
                    ProductsOptions = products |> Include
                    ServiceLocations = None
                    TestTypes = None
                    VariablesSeq = Some variableshit1
                },
                output1
            )
            |> Hit

            (
                {
                    ProductsOptions = products |> Exclude
                    ServiceLocations = None
                    TestTypes = None
                    VariablesSeq = Some variablesNoHit1
                }
            )
            |> NoHit
        ]
   
    
    let cases =
        verificationSetups
        |> Seq.map (fun case ->
            {
                Name = name
                Case = case
            })

    let filterCases cases isHit =
        cases
        |> Seq.filter (fun case ->
            match case.Case with 
            | Hit _ -> isHit
            | NoHit _ -> not(isHit))
    
    let hitVerInstruction =
        filterCases cases true
        |> fun newCases ->
            Seq.zip newCases [1..newCases |> Seq.length]
        |> Seq.map (fun (case,num) ->
            write2MarkDown case ("hit " + (num |> string))
            )
        |> String.concat ""

    let NohitVerInstruction =
        filterCases cases false
        |> fun newCases ->
            Seq.zip newCases [1..newCases |> Seq.length]
        |> Seq.map (fun (case,num) ->
            write2MarkDown case ("no hit " + (num |> string))
            )
        |> String.concat ""
    
    let finalVerIntructionBoth =
        hitVerInstruction + NohitVerInstruction

module SK_2_40 =

    let name1 = "Generic_2/-40_B"
    let name2 = "RX_RF_LO_2/-40_B"
    let products =
        seq[
            ("KRC 161*" |> AllRStates )
            ("KRD 901*" |> AllRStates )
        ]

    let serviceLocations =
        seq[
            "JP-SYNNR"
            "MX-SANGU"
            "MX-JABGU"
            "BD-SYNDH"
            "TW-ERITB"
            "IN-JABPU"
            "CN-JABWU"
            "SE-SYNGO"
        ]

    let pqatLiefCycleCompChange =
        "{
        \"SerialNo\": \"E23A550430\",
        \"Time\": \"Jan  8 2020 12:00:00:000AM\",
        \"TestPointGroupName\": \"\",
        \"TestPointName\": \"\",
        \"MeasuredValue\": \"\",
        \"Component\": \"\",
        \"ComponentPos\": \"\",
        \"Fault\": \"-\",
        \"RCO\": \"15625-KRC161619/1-4_B\",
        \"Action\": \"A130\"
        }"

    let pqatLiefCycleCompChangeHit =
        "{
        \"SerialNo\": \"E23A550430\",
        \"Time\": \"Jan  8 2020 12:00:00:000AM\",
        \"TestPointGroupName\": \"\",
        \"TestPointName\": \"\",
        \"MeasuredValue\": \"\",
        \"Component\": \"\",
        \"ComponentPos\": \"\",
        \"Fault\": \"-\",
        \"RCO\": \"This field should NOT contain: '15625-KRC161619/1-4','15625-KRC161650/1-1','15625-KRC161753/1-1','15625-KRC161707/2-1'\",
        \"Action\": \"A130\"
        }"
    let pqatLatIpAnswerHit =
        "[
            {
                \"Question\": \"E23A550430\",
                \"Answer\": \"15625-KRC161619/1-4\"
            }
        ]"

    let pqatLatIpAnswerNoHit =
        "[
            {
                \"Question\": \"E23A550430\",
                \"Answer\": \"This field should NOT be equal to: '15625-KRC161619/1-4','15625-KRC161650/1-1','15625-KRC161753/1-1','15625-KRC161707/2-1_E'\"
            }
        ]"

    let pqatLifeCycleComponentChangeMsg str =
          "A PQAT life cycle component change query list should be obtained, " + str + " of which should have a RCO entry such that the matched LAT IP answer above **contains** any of the RCO:s as shown in the json string below"
   
    let pqatLifeCycleComponentChangeMsgHit =
        pqatLifeCycleComponentChangeMsg "**none**"

    let pqatLifeCycleComponentChangeMsgNoHit =
        pqatLifeCycleComponentChangeMsg "one or more "

    let latIpAnswerMsghit =
        "Use a serial number, for instance E23A550430; E23A186916, such that you get **EXACTLY** one of the following **PQAT LAT IP** answers, see also json eaxmple below:\n\t\t
        ```json\n\t\t
            15625-KRC161619/1-4  \n   
            15625-KRC161650/1-1  \n
            15625-KRC161753/1-1  \n
            15625-KRC161707/2-1_E\n\t\t
            ```"
    let latIpAnswerMsgNohit =
        "Use a serial number, for instance E23A550430; E23A186916, such that you get **NONE** one of the following **PQAT LAT IP** answers, see also json example below:
        ```json\n\t\t
        15625-KRC161619/1-4  \n   
        15625-KRC161650/1-1  \n
        15625-KRC161753/1-1  \n
        15625-KRC161707/2-1_E\n\t\t
        ```"
    
    let variableshit1 =
        seq[
            seq[ 
                (
                    (Some pqatLatIpAnswerHit, latIpAnswerMsghit) |> Pqat
                )   
                (
                    (Some pqatLiefCycleCompChangeHit, pqatLifeCycleComponentChangeMsgHit) |> Pqat
                )  
            ]
        ]

    let variableshit2 =
        seq[
            seq[ 
                (
                    (Some pqatLatIpAnswerHit, latIpAnswerMsghit) |> Pqat
                )   
                (
                    (Some pqatLiefCycleCompChangeHit, pqatLifeCycleComponentChangeMsgHit) |> Pqat
                )  
                (
                    ("RX RF LO 1 out of lock",Some("1 or more times"), "HWlog.txt") |> Log
                )   
            ]
        ]

    let variablesNoHit1 =
        seq[
            seq[
                (
                    (Some pqatLatIpAnswerHit, latIpAnswerMsghit) |> Pqat
                )   
                (
                    (Some pqatLiefCycleCompChangeHit, pqatLifeCycleComponentChangeMsgNoHit) |> Pqat
                )   
            ]
            seq[
                (
                    (Some pqatLatIpAnswerNoHit, latIpAnswerMsgNohit) |> Pqat
                )   
            ]
        ]

    let getouputTxt modString =
        "Unit is affected by 15625-KRC161707/2-1_E.\n
        Please replace RJC5200112/104 on positions C171G0M, C171G0S, C109G0M, C109G0S\n
        and replace ROR1010030/AB on positions C171G0M, C171G0S, C109G0M, C109G0S, C171G0M, C171G0S, C109G0M, C109G0S.\n
        Report as:\n
        A130 15625-KRC161707/2-1_E\n
        " + modString + "RJC5200112/104 C171G0M\n
        " + modString + "RJC5200112/104 C171G0S\n
        " + modString + "RJC5200112/104 C109G0M\n
        " + modString + "RJC5200112/104 C109G0S\n
        " + modString + "ROR1010030/AB C171G0M \n
        " + modString + "ROR1010030/AB C171G0S \n
        " + modString + "ROR1010030/AB C109G0M \n
        " + modString + "ROR1010030/AB C109G0S \n
        " + modString + "ROR1010030/AB C171G0M \n
        " + modString + "ROR1010030/AB C171G0S \n
        " + modString + "ROR1010030/AB C109G0M \n
        " + modString + "ROR1010030/AB C109G0S"

    let outputTxt1 = getouputTxt "A105/59" |> fun x -> seq[x |> Output]  
    let outputTxt2 = getouputTxt "A105/48" |> fun x -> seq[x |> Output]


    let verificationSetups =
        seq[
            (
                {
                    ProductsOptions = (products |> Include)
                    ServiceLocations = serviceLocations |> (Excluded >> Some)
                    TestTypes = None
                    VariablesSeq = Some variableshit1
                },
                outputTxt2
            )
            |> Hit

            (
                {
                    ProductsOptions = products |> Exclude
                    ServiceLocations = None
                    TestTypes =  None
                    VariablesSeq = Some variableshit1
                }
            )
            |> NoHit

            (
                {
                    ProductsOptions = (products |> Include)
                    ServiceLocations = serviceLocations |> (Excluded >> Some)
                    TestTypes = None
                    VariablesSeq = Some variableshit2
                },
                outputTxt1
            )
            |> Hit

            (
                {
                    ProductsOptions = products |> Include
                    ServiceLocations = serviceLocations |> (Included >> Some)
                    TestTypes = None
                    VariablesSeq = None
                }
            )
            |> NoHit
        ]
   
    
    let cases =
        Seq.zip verificationSetups (seq[name1;name1;name2;name1 + " and " + name2])
        |> Seq.map (fun (case,name) ->
            {
                Name = name
                Case = case
            })

    let filterCases cases isHit =
        cases
        |> Seq.filter (fun case ->
            match case.Case with 
            | Hit _ -> isHit
            | NoHit _ -> not(isHit))
    
    let hitVerInstruction =
        filterCases cases true
        |> fun newCases ->
            Seq.zip newCases [1..newCases |> Seq.length]
        |> Seq.map (fun (case,num) ->
            write2MarkDown case ("hit " + (num |> string))
            )
        |> String.concat ""

    let NohitVerInstruction =
        filterCases cases false
        |> fun newCases ->
            Seq.zip newCases [1..newCases |> Seq.length]
        |> Seq.map (fun (case,num) ->
            write2MarkDown case ("no hit " + (num |> string))
            )
        |> String.concat ""
    
    let finalVerIntructionBoth =
        hitVerInstruction + NohitVerInstruction

module SK_1_82 =

    let name = "LAT 1/-82, Rule 1/2/3/4, Rule for EMCA HW faults on ERS BB - CM memory, Rev A"
    let products =
        seq[
            ("KDU 137 925/41" |> AllRStates )
            ("KDV 127 620/11" |> AllRStates )
            ("KDV 127 621/11" |> AllRStates )
            ("KDU 137 847/11" |> AllRStates )
            ("KDU 137 848/11" |> AllRStates )
            ("KDU 137 0053/31" |> AllRStates )
        ]

    let logFiles phrase = "syslog " + phrase + " llog"
    let variableshit1 =
        seq[
            seq[ 
                (
                    ("Emca 1:Unable to read EMCA fatalerror",Some("1 or more times"), logFiles "or") |> Log
                )  
            ]
            seq[ 
                (
                    ("Emca 2:DSP 12: \"CM memory header",Some("1 or more times"), logFiles "or") |> Log
                )  
            ]
            seq[ 
                (
                    ("Emca 3:DSP 12: \"Invalid CM\/EM address",Some("1 or more times"), logFiles "or") |> Log
                )  
            ]
            seq[ 
                (
                    ("Emca 1:DSP 12: \"Trying to free a cm",Some("1 or more times"), logFiles "or") |> Log
                )  
            ]
            seq[ 
                (
                    ("Emca 2:DSP 12: \"External memory tail buffer",Some("1 or more times"), logFiles "or") |> Log
                )  
            ]
        ]

    let variablesNoHit1 =
        seq[
            seq[ 
                (
                    ("Emca 1:Unable to read EMCA fatalerror",None, logFiles "and") |> Log
                )  

                (
                    ("Emca 2:DSP 12: \"CM memory header",None, logFiles "and") |> Log
                )  

                (
                    ("Emca 3:DSP 12: \"Invalid CM\/EM address",None, logFiles "and") |> Log
                )  

                (
                    ("Emca 1:DSP 12: \"Trying to free a cm",None, logFiles "and") |> Log
                )  

                (
                    ("Emca 2:DSP 12: \"External memory tail buffer",None, logFiles "and") |> Log
                )  
            ]
        ]
    
    let output1 = 
        seq[
            (
                "HW Fault identified. Replace BBM/Trinity at position D1000T1. Report as A105/59. Major Fault."
                |> Output
            )
            (
                "HW Fault identified. Replace BBM/Trinity at position D1000T2. Report as A105/59. Major Fault."
                |> Output
            )
            (
                "HW Fault identified. Replace BBM/Trinity at position D1000T3. Report as A105/59. Major Fault."
                |> Output
            )
            (
                "HW Fault identified. Replace BBM/Trinity at position D1000T1. Report as A105/59. Major Fault."
                |> Output
            )
            (
                "HW Fault identified. Replace BBM/Trinity at position D1000T2. Report as A105/59. Major Fault."
                |> Output
            )
        ]

    let verificationSetups =
        seq[
            (
                {
                    ProductsOptions = products |> Productoptions.Include
                    ServiceLocations = None
                    TestTypes = None
                    VariablesSeq = Some variableshit1
                },
                output1
            )
            |> Hit

            (
                {
                    ProductsOptions = products |> Productoptions.Include
                    ServiceLocations = None
                    TestTypes = None
                    VariablesSeq = Some variablesNoHit1
                }
            )
            |> NoHit
        ]
   
    
    let cases =
        verificationSetups
        |> Seq.map (fun case ->
            {
                Name = name
                Case = case
            })

    let filterCases cases isHit =
        cases
        |> Seq.filter (fun case ->
            match case.Case with 
            | Hit _ -> isHit
            | NoHit _ -> not(isHit))
    
    let hitVerInstruction =
        filterCases cases true
        |> fun newCases ->
            Seq.zip newCases [1..newCases |> Seq.length]
        |> Seq.map (fun (case,num) ->
            write2MarkDown case ("hit " + (num |> string))
            )
        |> String.concat ""

    let NohitVerInstruction =
        filterCases cases false
        |> fun newCases ->
            Seq.zip newCases [1..newCases |> Seq.length]
        |> Seq.map (fun (case,num) ->
            write2MarkDown case ("no hit " + (num |> string))
            )
        |> String.concat ""
    
    let finalVerIntructionBoth =
        hitVerInstruction + NohitVerInstruction

module SK_1_73 =

    let name = "LAT 1/-73, Mock counter too high for BB G2.1, Rev A"
    let products =
        seq[
            (("KDU 137 925/31","R9","-")  |> RstateFromTo)
            ( ("KDU 137 925/41","R9","-" ) |> RstateFromTo )
            ( "KDU 137 862/11"   |> AllRStates )
            ( "KDU 137 974/11"   |> AllRStates )
            ( "KDU 137 847/11"   |> AllRStates )
            ( "KDU 137 848/11 "  |> AllRStates )
            ( "KDU 137 0053/31 " |> AllRStates )
        ]

    let testTypes =
        seq[
            "RcPrtt"
            "RcExtPrtt"
            "ScPrtt"
        ]

    let warningOrCorrectMsg phrase =
        "Look for files **/tmp/ee_esi/ee-ptad-expiry/soft_fuse_info** or **/tmp/ee_esi/ee-ptad-expiry/hard_fuse_info**,
        both files should exist and contain (only) a decimal number in ASCII, see table below.
        This number should be converted to an integer number, hencforth denoted as *val_actual*.
        Now, in order to get a criteria hit, the **final condition**, see below, must be fullfilled " + phrase + ".
        Note that no text ouput is to be displayed on the pdf traveller, the criteria is information only, 
        a hit indication can be viewed in **Idefix output window** or in the **json result file**.\n\t\t
        <style type=\"text/css\">
        .tg  {border-collapse:collapse;border-spacing:0;}
        .tg td{font-family:Arial, sans-serif;font-size:23px;padding:10px 5px;border-style:solid;border-width:1px;overflow:hidden;word-break:normal;border-color:black;}
        .tg th{font-family:Arial, sans-serif;font-size:23px;font-weight:normal;padding:10px 5px;border-style:solid;border-width:1px;overflow:hidden;word-break:normal;border-color:black;}
        .tg .tg-0pky{border-color:inherit;text-align:left;vertical-align:top}
        .tg .tg-0lax{text-align:left;vertical-align:top}
        </style>
        <table class=\"tg\">
          <tr>
            <th class=\"tg-0pky\">integer</th>
            <th class=\"tg-0pky\">decimal (expected content in file)</th>
          </tr>
          <tr>
            <td class=\"tg-0pky\">1</td>
            <td class=\"tg-0pky\">49</td>
          </tr>
          <tr>
            <td class=\"tg-0pky\">2</td>
            <td class=\"tg-0pky\">50</td>
          </tr>
          <tr>
            <td class=\"tg-0pky\">3</td>
            <td class=\"tg-0pky\">51</td>
          </tr>
          <tr>
            <td class=\"tg-0lax\">4</td>
            <td class=\"tg-0lax\">52</td>
          </tr>
          <tr>
            <td class=\"tg-0lax\">5</td>
            <td class=\"tg-0lax\">53</td>
          </tr>
          <tr>
            <td class=\"tg-0lax\">6</td>
            <td class=\"tg-0lax\">54</td>
          </tr>
          <tr>
            <td class=\"tg-0lax\">7</td>
            <td class=\"tg-0lax\">55</td>
          </tr>
          <tr>
            <td class=\"tg-0lax\">8</td>
            <td class=\"tg-0lax\">56</td>
          </tr>
          <tr>
            <td class=\"tg-0lax\">9</td>
            <td class=\"tg-0lax\">57</td>
         </tr>
        </table>\n"

    let warningOrCorrectEquation cond =
        "month = the current month\n
        year = the current year\n
        local calendar_quarter = (month - 1) / 3  +  1)\n
        local year_since_2019 = year - 2019\n
        floor(n) -> function that converts the number n to the lowest integer, for instance 4.8 to 4 or 2.2 to 2\n
        val_actual = floor(year_since_2019 * 4 + calendar_quarter - 1)\n
        final condition:\n
        val_actual " + (cond) + " val_expected"

    let errorMsg =
        "Look for file **/tmp/ee_esi/ee-ptad-expiry/error in the ESI archive**. 
        if it exists, the criteria is hit.
        Note that no text ouput is to be displayed on the pdf traveller, the criteria is information only, 
        a hit indication can be viewed in **Idefix output window** or in the **json result file**."

    let noHitMsg =
        "None of the files **/tmp/ee_esi/ee-ptad-expiry/soft_fuse_info**,**/tmp/ee_esi/ee-ptad-expiry/hard_fuse_info**,**/tmp/ee_esi/ee-ptad-expiry/error in the ESI archive**, are found."
    
    let phrase2 = "for at lease **one** of the files. Furthermore, in a case where one of the files does **not** fullfill the mentioned condition, the file in question must contain a valid decimal ASCII number"
    let variableshit1 =
        seq[
            seq[ 
                (
                    (Some (warningOrCorrectEquation "=="), warningOrCorrectMsg "for **both** files") |> Pqat
                )  
            ]
            seq[ 
                (
                    (Some (warningOrCorrectEquation ">"),warningOrCorrectMsg phrase2) |> Pqat
                )  
            ]
            seq[ 
                (
                    ( None, errorMsg) |> Pqat
                )  
            ]
        ]

    let variablesNoHit1 =
        seq[
            seq[
                (
                    ( None, noHitMsg) |> Pqat
                )   
            ]
        ]
    
    let output1 = 
        seq[
            (
                "Correct_1/7-73_A (should not be displayed in pdf traveller)"
                |> Output
            )  
            (
                "Warning_1/-73 (should not be displayed in pdf traveller)"
                |> Output
            )  
            (
                "Error_1-73_A (should not be displayed in pdf traveller)"
                |> Output
            )  
        ]

    let verificationSetups =
        seq[
            (
                {
                    ProductsOptions = products |> Productoptions.Include
                    ServiceLocations = None
                    TestTypes = testTypes |> ( TestTypes.ExcludedType >> Some )
                    VariablesSeq = Some variableshit1
                },
                output1
            )
            |> Hit

            (
                {
                    ProductsOptions = products |> Productoptions.Include
                    ServiceLocations = None
                    TestTypes = testTypes |> ( TestTypes.ExcludedType >> Some )
                    VariablesSeq = Some variablesNoHit1
                }
            )
            |> NoHit

            (
                {
                    ProductsOptions = products |> Productoptions.Include
                    ServiceLocations = None
                    TestTypes = testTypes |> ( TestTypes.IncludedType >> Some )
                    VariablesSeq = None
                }
            )
            |> NoHit
        ]
   
    
    let cases =
        verificationSetups
        |> Seq.map (fun case ->
            {
                Name = name
                Case = case
            })

    let filterCases cases isHit =
        cases
        |> Seq.filter (fun case ->
            match case.Case with 
            | Hit _ -> isHit
            | NoHit _ -> not(isHit))
    
    let hitVerInstruction =
        filterCases cases true
        |> fun newCases ->
            Seq.zip newCases [1..newCases |> Seq.length]
        |> Seq.map (fun (case,num) ->
            write2MarkDown case ("hit " + (num |> string))
            )
        |> String.concat ""

    let NohitVerInstruction =
        filterCases cases false
        |> fun newCases ->
            Seq.zip newCases [1..newCases |> Seq.length]
        |> Seq.map (fun (case,num) ->
            write2MarkDown case ("no hit " + (num |> string))
            )
        |> String.concat ""
    
    let finalVerIntructionBoth =
        hitVerInstruction + NohitVerInstruction

module SK_1_15 =

    let name = "Replacement of DSPs in DUWv2 due to 'n/RxBcThread red led on"
    let products =
        seq[
            ("KDU127174/1 " |> AllRStates )
            ("KDU127174/3" |> AllRStates )
            ("KDU127174/4" |> AllRStates )
        ]
    
    let variableshit1 =
        seq[
            seq[ 
                (
                    ("3/RxBcThread on", Some("more than 2 times"), "dumpelg.txt") |> VariableType.Log
                )
                (
                    ("Failed to load DSP on SNID 819", Some("more than once"), "dumpelg.txt") |> VariableType.Log
                )
                (
                    ("MCK;", None, "dumpelg.txt") |> Log
                )
                (
                    ("2AC;", None, "dumpelg.txt") |> Log
                )
                (
                    ("Failed to load DSP on SNID 84", None, "dumpelg.txt") |> Log
                )
                (
                    ("Fatal bit error detected in DSP 84", None, "dumpelg.txt") |> Log
                )
            ]
            seq[ 
                (
                    ("4/RxBcThread on", Some("more than 2 times"), "dumpelg.txt") |> VariableType.Log
                )
                (
                    ("Fatal bit error detected in DSP 87", Some("more than once"), "dumpelg.txt") |> VariableType.Log
                )
                (
                    ("MCK;", None, "dumpelg.txt") |> Log
                )
                (
                    ("2AC;", None, "dumpelg.txt") |> Log
                )
                (
                    ("Failed to load DSP on SNID 896", None, "dumpelg.txt") |> Log
                )
                (
                    ("Fatal bit error detected in DSP 896", None, "dumpelg.txt") |> Log
                )
            ]
            seq[ 
                (
                    ("5/RxBcThread on", Some("more than 2 times"), "dumpelg.txt") |> VariableType.Log
                )
                (
                    ("Failed to load DSP on SNID 1331", Some("more than once"), "dumpelg.txt") |> VariableType.Log
                )
                (
                    ("MCK;", None, "dumpelg.txt") |> Log
                )
                (
                    ("2AC;", None, "dumpelg.txt") |> Log
                )
                (
                    ("Failed to load DSP on SNID 135", None, "dumpelg.txt") |> Log
                )
                (
                    ("Fatal bit error detected in DSP 135", None, "dumpelg.txt") |> Log
                )
            ]
            seq[ 
                (
                    ("6/RxBcThread on", Some("more than 2 times"), "dumpelg.txt") |> VariableType.Log
                )
                (
                    ("Fatal bit error detected in DSP 1331", Some("more than once"), "dumpelg.txt") |> VariableType.Log
                )
                (
                    ("MCK;", None, "dumpelg.txt") |> Log
                )
                (
                    ("2AC;", None, "dumpelg.txt") |> Log
                )
                (
                    ("Failed to load DSP on SNID 135", None, "dumpelg.txt") |> Log
                )
                (
                    ("Fatal bit error detected in DSP 135", None, "dumpelg.txt") |> Log
                )
            ]
            seq[ 
                (
                    ("7/RxBcThread on", Some("more than 2 times"), "dumpelg.txt") |> VariableType.Log
                )
                (
                    ("Fatal bit error detected in DSP 1459", Some("more than once"), "dumpelg.txt") |> VariableType.Log
                )
                (
                    ("MCK;", None, "dumpelg.txt") |> Log
                )
                (
                    ("2AC;", None, "dumpelg.txt") |> Log
                )
                (
                    ("Failed to load DSP on SNID 148", None, "dumpelg.txt") |> Log
                )
                (
                    ("Fatal bit error detected in DSP 148", None, "dumpelg.txt") |> Log
                )
            ]
            seq[ 
                (
                    ("8/RxBcThread on", Some("more than 2 times"), "dumpelg.txt") |> VariableType.Log
                )
                (
                    ("Failed to load DSP on SNID 153", Some("more than once"), "dumpelg.txt") |> VariableType.Log
                )
                (
                    ("MCK;", None, "dumpelg.txt") |> Log
                )
                (
                    ("2AC;", None, "dumpelg.txt") |> Log
                )
                (
                    ("Fatal bit error detected in DSP 151", None, "dumpelg.txt") |> Log
                )
                (
                    ("Failed to load DSP on SNID 151", None, "dumpelg.txt") |> Log
                )
            ]
            seq[ 
                (
                    ("8/RxBcThread on", Some("more than 2 times"), "dumpelg.txt") |> VariableType.Log
                )
                (
                    ("Fatal bit error detected in DSP 151", Some("more than once"), "dumpelg.txt") |> VariableType.Log
                )
                (
                    ("MCK;", None, "dumpelg.txt") |> Log
                )
                (
                    ("2AC;", None, "dumpelg.txt") |> Log
                )
                (
                    ("Failed to load DSP on SNID 153", None, "dumpelg.txt") |> Log
                )
                (
                    ("Fatal bit error detected in DSP 153", None, "dumpelg.txt") |> Log
                )
            ]
        ]

    let variablesNohit1 =
        seq[
            seq[ 
                (
                    ("3/RxBcThread on", Some("**EXACTLY** one time"), "dumpelg.txt") |> VariableType.Log
                )
                (
                    ("Failed to load DSP on SNID 819", Some("more than once"), "dumpelg.txt") |> VariableType.Log
                )
                (
                    ("MCK;", None, "dumpelg.txt") |> Log
                )
                (
                    ("2AC;", None, "dumpelg.txt") |> Log
                )
                (
                    ("Failed to load DSP on SNID 84", None, "dumpelg.txt") |> Log
                )
                (
                    ("Fatal bit error detected in DSP 84", None, "dumpelg.txt") |> Log
                )
            ]
            seq[ 
                (
                    ("3/RxBcThread on", Some("more than 2 times"), "dumpelg.txt") |> VariableType.Log
                )
                (
                    ("Failed to load DSP on SNID 819", None, "dumpelg.txt") |> VariableType.Log
                )
                (
                    ("MCK;", None, "dumpelg.txt") |> Log
                )
                (
                    ("2AC;", None, "dumpelg.txt") |> Log
                )
                (
                    ("Failed to load DSP on SNID 84", None, "dumpelg.txt") |> Log
                )
                (
                    ("Fatal bit error detected in DSP 84", None, "dumpelg.txt") |> Log
                )
            ]
            seq[ 
                (
                    ("3/RxBcThread on", Some("more than 2 times"), "dumpelg.txt") |> VariableType.Log
                )
                (
                    ("Failed to load DSP on SNID 819", Some("more than once"), "dumpelg.txt") |> VariableType.Log
                )
                (
                    ("MCK;", Some("more than once"), "dumpelg.txt") |> Log
                )
                (
                    ("2AC;", Some("more than once"), "dumpelg.txt") |> Log
                )
                (
                    ("Failed to load DSP on SNID 84", None, "dumpelg.txt") |> Log
                )
                (
                    ("Fatal bit error detected in DSP 84", None, "dumpelg.txt") |> Log
                )
            ]
        ]
    
    let output1 = 
        seq[
            (
                "Replace Nyquist D2000D3 on DUW-Main ROA 128 4755. 
        Report as A105/59."
                |> Output
            )  
            (
                "Replace Nyquist D2000D5 on DUW-Main ROA 128 4755.
        Report as A105/59."
                |> Output
            )  
            (
                "Replace Nyquist D2000D2 on DUW-Extension ROA 128 4756.
        Report as A105/59."
                |> Output
            )  
            (
                "Replace Nyquist D2000D5 on DUW-Extension ROA 128 4756.
        Report as A105/59."
                |> Output
            )  
            (
                "Replace Nyquist D2000D7 on DUW-Extension ROA 128 4756.
        Report as A105/59"
                |> Output
            )  
            (
                "Replace Nyquist D2000D10 on DUW-Extension ROA 128 4756.
        Report as A105/59."
                |> Output
            )  
            (
                "Replace Nyquist D2000D9 on DUW-Extension ROA 128 4756.
        Report as A105/59"
                |> Output
            ) 
        ]

    let verificationSetups =
        seq[
            (
                {
                    ProductsOptions = products |> Productoptions.Include
                    ServiceLocations = None
                    TestTypes = None
                    VariablesSeq = Some variableshit1
                },
                output1
            )
            |> Hit

            (
                {
                    ProductsOptions = products |> Productoptions.Include
                    ServiceLocations = None
                    TestTypes = None
                    VariablesSeq = Some variablesNohit1
                }
            )
            |> NoHit
        ]
   
    
    let cases =
        verificationSetups
        |> Seq.map (fun case ->
            {
                Name = name
                Case = case
            })

    let filterCases cases isHit =
        cases
        |> Seq.filter (fun case ->
            match case.Case with 
            | Hit _ -> isHit
            | NoHit _ -> not(isHit))
    
    let hitVerInstruction =
        filterCases cases true
        |> fun newCases ->
            Seq.zip newCases [1..newCases |> Seq.length]
        |> Seq.map (fun (case,num) ->
            write2MarkDown case ("hit " + (num |> string))
            )
        |> String.concat ""

    let NohitVerInstruction =
        filterCases cases false
        |> fun newCases ->
            Seq.zip newCases [1..newCases |> Seq.length]
        |> Seq.map (fun (case,num) ->
            write2MarkDown case ("no hit " + (num |> string))
            )
        |> String.concat ""
    
    let finalVerIntructionBoth =
        hitVerInstruction + NohitVerInstruction

module SK_2_27 =

    let name = "DCDC Component Failure indication"
    let products =
        seq[
            ("KRC 118 56/1" |> AllRStates)
            ("KRC 118 72/1" |> AllRStates)
            ("KRC 118 64/1" |> AllRStates)
            ("KRC 118 64/2" |> AllRStates)
            ("KRC 118 64/3" |> AllRStates)
            ("KRC 118 70/2" |> AllRStates)
            ("KRC 118 70/3" |> AllRStates)
            ("KRC 118 46/2" |> AllRStates)
            ("KRC 118 30/1" |> AllRStates)
        ]

    let messageWithTable =
        "The following text output is displayed whenever a criteria hit occurs:
        ```plain
            Fault Ind(s) {Log} present in HW Log. Replace components at position {TRX} and position {PA}.
            Report both as A105/59
        ```
        Here, *TRX* and *PA* is the board position corresponding to the product being tested, see table below. *Log* is the actual log within HW Log file causing the criteria hit, which could be either one or all of the following:
            - **317;**
            - **837;**
        
            |    Product   Number    |    Product   Name    |    Part   Number    |    TRX     board position    |    PA      board position    |
            |------------------------|----------------------|---------------------|------------------------------|------------------------------|
            |    KRC11856/1          |    RUL 01 B13        |    RYT1136444/1     |    N5400                     |    N5100                     |
            |    KRC11872/1          |    RRUS 01 B0        |    RYT1136444/1     |    N5400                     |    N5100                     |
            |    KRC11864/1          |    RUS 01 B5         |    RYT1136444/1     |    N5400                     |    N5100                     |
            |    KRC11864/2          |    RUS 01 B5         |    RYT1136444/1     |    N5400                     |    N5100                     |
            |    KRC11864/3          |    RUS 01 B5         |    RYT1136444/1     |    N5400                     |    N5100                     |
            |    KRC11870/2          |    RRUS 01 B5        |    RYT1136444/1     |    N5400                     |    N5100                     |
            |    KRC11870/3          |    RRUS 01 B5        |    RYT1136444/1     |    N5400                     |    N5100                     |
            |    KRC11846/2          |    RRUW 02 B5        |    RYT1136444/1     |    N5400                     |    N5100                     |
            |    KRC11830/1          |    RUS 01 B7         |    RYT1136444/1     |    N5400                     |    N5100                     |
            |                        |                      |                     |                              |                              |
        "
    
    let variableshit1 =
        seq[
            seq[ 
                (
                    ( None, messageWithTable) |> Pqat
                )  
                (
                    ("317;", Some("more than once"), "HWLog.txt") |> VariableType.Log
                )
            ]
            seq[ 
                (
                    ( None, messageWithTable) |> Pqat
                )  
                (
                    ("837;", Some("more than once"), "HWLog.txt") |> VariableType.Log
                )
            ]
            seq[ 
                (
                    ( None, messageWithTable) |> Pqat
                )  
                (
                    ("317;", Some("more than once"), "HWLog.txt") |> VariableType.Log
                )
                (
                    ("837;", Some("more than once"), "HWLog.txt") |> VariableType.Log
                )
            ]
        ]

    let variablesNohit1 =
        seq[
            seq[ 
                (
                    ("317;", None, "HWLog.txt") |> VariableType.Log
                )
                (
                    ("837;", None, "HWLog.txt") |> VariableType.Log
                )
            ]
        ]
    
    let output1 = 
        seq[
            (
                "Fault Ind 317 present in HW Log. Replace components at 
        position TRX and position PA. Report both as A105/59"
                |> Output
            )  
            (
                "Fault Ind 837 present in HW Log. Replace components at 
        position TRX and position PA. Report both as A105/59"
                |> Output
            )
            (
                "Fault Inds 317 and 837 present in HW Log. Replace components at 
        position TRX and position PA. Report both as A105/59"
                |> Output
            )
        ]

    let verificationSetups =
        seq[
            (
                {
                    ProductsOptions = products |> Productoptions.Include
                    ServiceLocations = None
                    TestTypes = None
                    VariablesSeq = Some variableshit1
                },
                output1
            )
            |> Hit

            (
                {
                    ProductsOptions = products |> Productoptions.Include
                    ServiceLocations = None
                    TestTypes = None
                    VariablesSeq = Some variablesNohit1
                }
            )
            |> NoHit
        ]
   
    
    let cases =
        verificationSetups
        |> Seq.map (fun case ->
            {
                Name = name
                Case = case
            })

    let filterCases cases isHit =
        cases
        |> Seq.filter (fun case ->
            match case.Case with 
            | Hit _ -> isHit
            | NoHit _ -> not(isHit))
    
    let hitVerInstruction =
        filterCases cases true
        |> fun newCases ->
            Seq.zip newCases [1..newCases |> Seq.length]
        |> Seq.map (fun (case,num) ->
            write2MarkDown case ("hit " + (num |> string))
            )
        |> String.concat ""

    let NohitVerInstruction =
        filterCases cases false
        |> fun newCases ->
            Seq.zip newCases [1..newCases |> Seq.length]
        |> Seq.map (fun (case,num) ->
            write2MarkDown case ("no hit " + (num |> string))
            )
        |> String.concat ""
    
    let finalVerIntructionBoth =
        hitVerInstruction + NohitVerInstruction

module SK_1_42 =

    let name = "Repeated Return BB52XX/BB66XX, 1/-42;F"
    let products =
        seq[
            ("KDU 137 847/*" |> AllRStates)
            ("KDV 127 620/*" |> AllRStates)
            ("KDU 137 848/*" |> AllRStates)
            ("KDV 127 621/*" |> AllRStates)
        ]
    
    let serviceLocations =
        seq[
            "MY-ERIKL"
            "EE-ERITA"
            "SE-SYNGA"
        ]

    let testTypes =
        seq[
            "RcLat"
        ]

    let messagePQAT input =
        "Here, we want serial numbers which will
            - Convert/translate a KDU serial number to KDV serial numbers, for instance TD3B317906 to TD3B317624:
                ```json
                [
                    {
                        \"TranslatedSerialNo\": \"TD3B317624\"
                    }
                ]
                ```
            - This translated serial number, in turn, has to provide a 'basicReturn' PQAT API response such as
                ```json
                [
                    {
                        \"ComplaintID\": \"APAC: 1113907937:1000189807:13\",
                        \"SerialNo\": \"CB4F004542\",
                        \"ProductNo\": \"KDU127161/3\",
                        \"Version\": \"R3C/B\",
                        \"CreationDate\": \"1970-01-01\",
                        \"ActionDate\": \"2019-08-04\",
                        \"ActionCode\": \"N001\",
                        \"RepairFlow\": \"HWS\",
                        \"FaultDescription\": \"NOINFO\",
                        \"ReparationDate\": \"2019-07-04\",
                        \"TotalReturnCount\": 5,
                        \"TotalHWSReturnCount\": 5,
                        \"CountryCode\": \"JP\",
                        \"EventDate\": \"2019-06-11\",
                        \"ReturnOrder\": 2
                    },
                    {
                        \"ComplaintID\": \"APAC: 1113907937:1000189807:13\",
                        \"SerialNo\": \"CB4F004542\",
                        \"ProductNo\": \"KDU127161/3\",
                        \"Version\": \"R3C/B\",
                        \"CreationDate\": \"1970-01-01\",
                        \"ActionDate\": \"2012-07-04\",
                        \"ActionCode\": \"N001\",
                        \"RepairFlow\": \"HWS\",
                        \"FaultDescription\": \"NOINFO\",
                        \"ReparationDate\": \"2019-07-04\",
                        \"TotalReturnCount\": 5,
                        \"TotalHWSReturnCount\": 5,
                        \"CountryCode\": \"JP\",
                        \"EventDate\": \"2019-06-09\",
                        \"ReturnOrder\": 2
                        }
                ]
                ```
        - A criteria will get a hit if, for the latest of the 'basicReturn' fields, the difference between
          *EventDate* field and *ReparationDate* field in amount of days is
          <= *Days*, see table below.

          |    Product                                    |    Service Location    |    Days    |    Originating country    |    Message    |
          |-----------------------------------------------|------------------------|------------|---------------------------|---------------|
          |    KDU137925/* (BB52xx)                       |    MY-ERIKL            |    185     |    *                      |    B          |
          |    KDU137925/* (BB52xx)                       |    EE-ERITA            |    185     |    *                      |    B          |
          |    KDU137847/* or KDV127620/*   (BB6620)      |    EE-ERITA            |    185     |    *                      |    B          |
          |    KDU137848/*   or   KDV127621/* (BB6630)    |    EE-ERITA            |    185     |    *                      |    B          |

        - Use the information above to " + input + " get a criteria hit.   
        "
    
    let serialNumbersHit =
        "No serial numbers found"
    let serialNumbersNoHit =
        "TD3B317790
        TD3B317796
        TD3B317793
        TD3B317794
        TD3B317798"
        
    let variableshit1 =
        seq[
            seq[ 
                (
                    ( None, messagePQAT "") |> Pqat
                )  
            ]
        ]

    let variablesNohit1 =
        seq[
            seq[ 
                (
                    ( None, messagePQAT "**NOT**") |> Pqat
                )  
            ]
        ]

    let outputMsg =
        "This unit shall be tested in an alternative sequence.
            1. First run ordinary PRTT test.
            
            	Important: If the R-state belongs to R1 versions, PRTT needs to be run with a manually selected SW level of 16B. 
            	In ordinary repair flow default SW should be used in PRTT.
            2. Run ordinary Final unit test.
            3. Store the following data in a folder named by date and serial number in the common agreed FTP or SFTP site:
            	a.	LAT data – HW Logs + system logs and ESI logs depending on what is applicable.
            	b.	Idefix PRTT test log summary.
            	c.	Idefix Module test log summary.
            
            		Troubleshooting is not allowed for failures identified in any of above test step unless RRAG reply with such a request.
            4. Inform the RRAG team of the repeated return by serial number and request evaluation of the unit.
            
            	The mail subject should be: 
            	Evaluate repeated return unit Serial Number before {todays date + 2 days}.
            
            	The mail should contain a path to the relevant folder in the FTP/SFTP site.
            
            	Send the information to
            	RRAG G2 &lt;PDLRRAGG2R@pdl.internal.ericsson.com&gt; mailbox 
            	and the 
            	Lead Repair Group mailbox &lt;eee.lead.repair@ericsson.com&gt; 
            	with high priority.
            5	If no response have been received within two working days - the unit shall be released in to normal repair flow starting with implementation of RCO:s."
    
    let output1 = 
        seq[
            (
                outputMsg
                |> Output
            )  
        ]

    let verificationSetups =
        seq[
            (
                {
                    ProductsOptions = products |> Productoptions.Include
                    ServiceLocations = serviceLocations |> ServiceLocations.Included |> Some
                    TestTypes = testTypes |> ( TestTypes.IncludedType >> Some )
                    VariablesSeq = Some variableshit1
                },
                output1
            )
            |> Hit

            (
                {
                    ProductsOptions = products |> Productoptions.Include
                    ServiceLocations = serviceLocations |> ServiceLocations.Included |> Some
                    TestTypes = testTypes |> ( TestTypes.ExcludedType >> Some )
                    VariablesSeq = Some variableshit1
                }
            )
            |> NoHit

            (
                {
                    ProductsOptions = products |> Productoptions.Include
                    ServiceLocations = serviceLocations |> ServiceLocations.Excluded |> Some
                    TestTypes = testTypes |> ( TestTypes.IncludedType >> Some )
                    VariablesSeq = Some variableshit1
                }
            )
            |> NoHit
            (
                {
                    ProductsOptions = products |> Productoptions.Include
                    ServiceLocations = serviceLocations |> ServiceLocations.Included |> Some
                    TestTypes = testTypes |> ( TestTypes.IncludedType >> Some )
                    VariablesSeq = Some variablesNohit1
                }
            )
            |> NoHit
        ]
   
    
    let cases =
        verificationSetups
        |> Seq.map (fun case ->
            {
                Name = name
                Case = case
            })

    let filterCases cases isHit =
        cases
        |> Seq.filter (fun case ->
            match case.Case with 
            | Hit _ -> isHit
            | NoHit _ -> not(isHit))
    
    let hitVerInstruction =
        filterCases cases true
        |> fun newCases ->
            Seq.zip newCases [1..newCases |> Seq.length]
        |> Seq.map (fun (case,num) ->
            write2MarkDown case ("hit " + (num |> string))
            )
        |> String.concat ""

    let NohitVerInstruction =
        filterCases cases false
        |> fun newCases ->
            Seq.zip newCases [1..newCases |> Seq.length]
        |> Seq.map (fun (case,num) ->
            write2MarkDown case ("no hit " + (num |> string))
            )
        |> String.concat ""
    
    let finalVerIntructionBoth =
        hitVerInstruction + NohitVerInstruction

module WriteVerification =
    SK_1_15.finalVerIntructionBoth +
    SK_2_27.finalVerIntructionBoth + 
    SK_1_42.finalVerIntructionBoth
    |> fun x ->
        File.WriteAllText("C:\Users\egoljos\Documents\Gitrepos\documentation\Aftermarket\Verification\Criteria_Ver_R10L.md",x)




open Vm
open Icgenerator

let getLoc name = int_of_string (String.sub name 4 ((String.length name) - 4))

let lookup el lst = try (snd (List.find (fun (el2,_) -> el = el2) lst)) with
                    | Not_found -> failwith "Element not found\n"

let rec printLabels labelEnv = match labelEnv with
    | label::remainingLabels -> (match label with
                                | (lb, position) -> Printf.printf "%s %d\n" lb position;
                                                    printLabels remainingLabels)
    | [] -> Printf.printf ""

let rec translateIRCExp exp = match exp with
  | IRC_IConst integer -> [PushS integer]
  | IRC_Plus (var1, var2) -> let loc1 = getLoc var1 in
                             let loc2 = getLoc var2 in
                             [PushToStack loc1; PushToStack loc2; Add]
  | IRC_Minus (var1, var2) -> let loc1 = getLoc var1 in
                         let loc2 = getLoc var2 in
                         [PushToStack loc1; PushToStack loc2; Sub]
  | IRC_Times (var1, var2) -> let loc1 = getLoc var1 in
                         let loc2 = getLoc var2 in
                         [PushToStack loc1; PushToStack loc2; Mult]
  | IRC_Division (var1, var2) -> let loc1 = getLoc var1 in
                             let loc2 = getLoc var2 in
                             [PushToStack loc1; PushToStack loc2; Div]
  | IRC_Var var -> let loc = getLoc var in
                   [PushToStack loc]
  (*| IRC_Not var -> let loc = getLoc var in*)
                   (*[PushToStack loc]*)

let rec translateCmd ir_list currentCmd labelEnv = match ir_list with
  | irc::remainingIrc -> (match irc with
                          | IRC_Assign (var, exp) -> let e1 = translateIRCExp exp in
                                                     let x = getLoc var in
                                                     let cmd = currentCmd @ e1 @ [AssignFromStack (1, x); PopS] in
                                                     translateCmd remainingIrc cmd labelEnv
                          | IRC_Print exp -> let e1 = translateIRCExp exp in
                                                     let cmd = currentCmd @ e1 @ [Output; PopS] in
                                                     translateCmd remainingIrc cmd labelEnv
                          | IRC_Label label -> let newLabelEnv = (string_of_int label, (List.length currentCmd))::labelEnv in
                                               printLabels newLabelEnv;
                                               translateCmd remainingIrc currentCmd newLabelEnv
                          | IRC_NonzeroJump (result, label) -> let position = lookup (string_of_int label) labelEnv in
                                                               let x = getLoc result in
                                                               let cmd = currentCmd @ [PushToStack x; NonZero position] in
                                                               translateCmd remainingIrc cmd labelEnv
                          | IRC_ZeroJump (result, label) -> printLabels labelEnv;
                                                            Printf.printf "%d\n" label;
                                                            let position = lookup (string_of_int label) labelEnv in
                                                            let x = getLoc result in
                                                            let cmd = currentCmd @ [PushToStack x; Zero position] in
                                                            translateCmd remainingIrc currentCmd labelEnv
                          | IRC_Goto label -> let position = lookup (string_of_int label) labelEnv in
                                              let cmd = currentCmd @ [Jump position] in
                                              translateCmd remainingIrc currentCmd labelEnv)
  | [] -> currentCmd

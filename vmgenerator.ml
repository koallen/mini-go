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
  | IRC_Eq (var1, var2) -> let loc1 = getLoc var1 in
                           let loc2 = getLoc var2 in
                           [PushToStack loc1; PushToStack loc2; Eq]
  | IRC_Gt (var1, var2) -> let loc1 = getLoc var1 in
                           let loc2 = getLoc var2 in
                           [PushToStack loc1; PushToStack loc2; Gt]

let rec translateCmd ir_list currentCmd = match ir_list with
  | irc::remainingIrc -> (match irc with
                          | IRC_Assign (var, exp) -> let e1 = translateIRCExp exp in
                                                     let x = getLoc var in
                                                     let cmd = currentCmd @ e1 @ [AssignFromStack (1, x); PopS] in
                                                     translateCmd remainingIrc cmd
                          | IRC_Print exp -> let e1 = translateIRCExp exp in
                                                     let cmd = currentCmd @ e1 @ [Output; PopS] in
                                                     translateCmd remainingIrc cmd
                          | IRC_Label label -> let cmd = currentCmd @ [Label label] in
                                               translateCmd remainingIrc cmd
                          | IRC_NonzeroJump (result, label) -> let x = getLoc result in
                                                               let cmd = currentCmd @ [PushToStack x; NonZero label] in
                                                               translateCmd remainingIrc cmd
                          | IRC_ZeroJump (result, label) -> let x = getLoc result in
                                                            let cmd = currentCmd @ [PushToStack x; Zero label] in
                                                            translateCmd remainingIrc cmd
                          | IRC_Goto label -> let cmd = currentCmd @ [Jump label] in
                                              translateCmd remainingIrc cmd)
  | [] -> currentCmd

let rec findLabel originalCmd label position = match originalCmd with
  | x::xs -> (match x with
              | Label l -> if (l == label) then (position + 1)
                           else findLabel xs label (position+1)
              | _ -> findLabel xs label (position+1))
  | [] -> -1

let rec updateJumps cmd newCmd originalCmd = match cmd with
  | x::xs -> (match x with
              | NonZero label -> let location = findLabel originalCmd label 0 in
                                 updateJumps xs (newCmd @ [NonZero location]) originalCmd
              | Zero label -> let location = findLabel originalCmd label 0 in
                              updateJumps xs (newCmd @ [Zero location]) originalCmd
              | Jump label -> let location = findLabel originalCmd label 0 in
                              updateJumps xs (newCmd @ [Jump location]) originalCmd
              | _ -> updateJumps xs (newCmd @ [x]) originalCmd)
  | [] -> newCmd

let rec prettyPrint cmd = match cmd with
  | x::xs -> (match x with
              | Label l -> Printf.printf "Label %d" l
              | _ -> Printf.printf "...\n")
  | [] -> Printf.printf "\n"

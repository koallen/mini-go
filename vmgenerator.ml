open Vm
open Icgenerator

let getLoc name = int_of_string (String.sub name 4 ((String.length name) - 4))

let rec translateExp exp = match exp with
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

let rec translateCmd ir_list currentCmd = match ir_list with
  | irc::remainingIrc -> (match irc with
                          | IRC_Assign (var, exp) -> let e1 = translateExp exp in
                                                     let x = getLoc var in
                                                     let cmd = currentCmd @ e1 @ [AssignFromStack (1, x); PopS] in
                                                     translateCmd remainingIrc cmd
                          | IRC_Print exp -> let e1 = translateExp exp in
                                                     let cmd = currentCmd @ e1 @ [Output; PopS] in
                                                     translateCmd remainingIrc cmd)
  | [] -> currentCmd

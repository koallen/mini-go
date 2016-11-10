open Vm
open Icgenerator

let rec prettyPrint cmd = match cmd with
  | x::xs -> Printf.printf "%s   " x;
             prettyPrint xs
  | [] -> Printf.printf "\n"

let contains s1 =
    let re = Str.regexp_string "temp" in
        try ignore (Str.search_forward re s1 0); true
        with Not_found -> false

let rec getStackLoc name locals loc = match locals with
  | x::xs -> if x = name then loc else getStackLoc name xs (loc + 1)
  | [] -> -1

let getLoc name locals = if (contains name) then
                           (let loc = int_of_string (String.sub name 4 ((String.length name) - 4)) in
                            [PushToStack loc])
                         else
                           (let loc = getStackLoc name locals 1 in
                            [AssignFromEnv (loc, 0); PushToStack 0])

let getLocAss name locals = if (contains name) then (int_of_string (String.sub name 4 ((String.length name) - 4)))
                            else getStackLoc name locals 1

let lookup el lst = try (snd (List.find (fun (el2,_) -> el = el2) lst)) with
                    | Not_found -> failwith "Element not found\n"

let rec printLabels labelEnv = match labelEnv with
    | label::remainingLabels -> (match label with
                                | (lb, position) -> Printf.printf "%s %d\n" lb position;
                                                    printLabels remainingLabels)
    | [] -> Printf.printf ""

let rec translateIRCExp exp locals = match exp with
  | IRC_IConst integer -> [PushS integer]
  | IRC_Plus (var1, var2) -> let loc1 = getLoc var1 locals in
                             let loc2 = getLoc var2 locals in
                             loc1@loc2@[Add]
  | IRC_Minus (var1, var2) -> let loc1 = getLoc var1 locals in
                              let loc2 = getLoc var2 locals in
                              loc1@loc2@[Sub]
  | IRC_Times (var1, var2) -> let loc1 = getLoc var1 locals in
                              let loc2 = getLoc var2 locals in
                              loc1@loc2@[Mult]
  | IRC_Division (var1, var2) -> let loc1 = getLoc var1 locals in
                                 let loc2 = getLoc var2 locals in
                                 loc1@loc2@[Div]
  | IRC_Var var -> getLoc var locals
  | IRC_Eq (var1, var2) -> let loc1 = getLoc var1 locals in
                           let loc2 = getLoc var2 locals in
                           loc1@loc2@[Eq]
  | IRC_Gt (var1, var2) -> let loc1 = getLoc var1 locals in
                           let loc2 = getLoc var2 locals in
                           loc1@loc2@[Gt]

let rec translateCmd ir_list currentCmd locals = match ir_list with
  | irc::remainingIrc -> (match irc with
                          | IRC_Assign (var, exp) -> let e1 = translateIRCExp exp locals in
                                                     let x = getLocAss var locals in
                                                     if (not (contains var)) then
                                                          (let cmd = currentCmd @ e1 @ [AssignFromStack (1, 0); PopS; UpdateToEnv (x, 0)] in
                                                          translateCmd remainingIrc cmd locals)
                                                     else
                                                          (let cmd = currentCmd @ e1 @ [AssignFromStack (1, x); PopS] in
                                                          translateCmd remainingIrc cmd locals)
                          | IRC_Print exp -> let e1 = translateIRCExp exp locals in
                                                     let cmd = currentCmd @ e1 @ [Output; PopS] in
                                                     translateCmd remainingIrc cmd locals
                          | IRC_Label label -> let cmd = currentCmd @ [Label label] in
                                               translateCmd remainingIrc cmd locals
                          | IRC_NonzeroJump (result, label) -> let x = getLocAss result locals in
                                                     if (not (contains result)) then
                                                         (let cmd = currentCmd @ [AssignFromEnv (x, 0); PushToStack 0; NonZero label] in
                                                          translateCmd remainingIrc cmd locals)
                                                     else
                                                         (let cmd = currentCmd @ [PushToStack x; NonZero label] in
                                                          translateCmd remainingIrc cmd locals)
                          | IRC_ZeroJump (result, label) -> let x = getLocAss result locals in
                                                     if (not (contains result)) then
                                                         (let cmd = currentCmd @ [AssignFromEnv (x, 0); PushToStack 0; Zero label] in
                                                          translateCmd remainingIrc cmd locals)
                                                     else
                                                         (let cmd = currentCmd @ [PushToStack x; Zero label] in
                                                          translateCmd remainingIrc cmd locals)
                          | IRC_Goto label -> let cmd = currentCmd @ [Jump label] in
                                              translateCmd remainingIrc cmd locals
                          | IRC_Param var -> let x = getLocAss var locals in
                                             let cmd = currentCmd @ [PushToEnv x] in
                                             translateCmd remainingIrc cmd locals
                          | IRC_Call (label, num_of_params) -> let address = (List.length currentCmd) + 2 in
                                                               let cmd = currentCmd @ [PushE address; Jump label] in
                                                               translateCmd remainingIrc cmd locals
                          | IRC_Get var -> let x = getLocAss var locals in
                                           let cmd = currentCmd @ [AssignFromEnv (1, x); PopE] in
                                           translateCmd remainingIrc cmd locals
                          | _ -> translateCmd remainingIrc currentCmd locals)
  | [] -> currentCmd

let rec addPushCommands num_of_locals cmds = match num_of_locals with
  | 0 -> cmds
  | _ -> addPushCommands (num_of_locals - 1) cmds@[PushE 0]

let rec addPopCommands num_of_locals cmds = match num_of_locals with
  | 0 -> cmds
  | _ -> addPopCommands (num_of_locals - 1) cmds@[PopE]

let rec appendToLocals old_locals new_locals num_of_locals = match num_of_locals with
  | 0 -> (match old_locals with
          | x::xs -> new_locals @ ["__returnaddress"] @ [x] @ xs)
  | _ -> (match old_locals with
          | x::xs -> appendToLocals xs (new_locals@[x]) (num_of_locals - 1))

let translateFunc proc currentCmd = match proc with
  | IRC_Proc (irc, locals, num_of_params) -> let num_of_locals = (List.length locals) - num_of_params in
                                             let pushLocalCode = addPushCommands num_of_locals [] in
                                             let new_locals = appendToLocals locals [] num_of_locals in
                                             let labelIRC = [List.nth irc 0] in
                                             let labelCMD = translateCmd labelIRC [] [] in
                                             let cmdWithLocal = currentCmd @ labelCMD @ pushLocalCode in
                                             (*Vm.printVMList cmdWithLocal 0;*)
                                             (match irc with
                                              | x::xs ->
                                             let cmd = translateCmd xs cmdWithLocal new_locals in
                                             let addressLoc = getLocAss "__returnaddress" new_locals in
                                             let returnAddrCode = [AssignFromEnv(addressLoc, 1)] in
                                             let popLocalCode = addPopCommands (List.length new_locals) [] in
                                             (match (List.nth irc ((List.length irc) - 1)) with
                                              | IRC_Return var -> let x = getLocAss var new_locals in
                                                                  if (contains var) then
                                                                      (let returnCmd = [PushToEnv x] in
                                                                       cmd @ returnAddrCode @ popLocalCode @ returnCmd @ [JumpMemLoc 1])
                                                                  else
                                                                       (cmd @ returnAddrCode @ [AssignFromEnv (x, 0)]
                                                                        @
                                                                        popLocalCode @ [PushToEnv 0; JumpMemLoc 1])
                                              | _ -> cmd @ returnAddrCode @ popLocalCode @ [JumpMemLoc 1]
                                             ))

let rec translateAllFuncs mainCmd procs = match procs with
  | x::xs -> let cmd = translateFunc x mainCmd in
             translateAllFuncs cmd xs
  | [] -> mainCmd

let translateMain proc = match proc with
  | IRC_Proc (irc, locals, _) -> let pushLocalCode = addPushCommands (List.length locals) [] in
                                 translateCmd irc pushLocalCode locals

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

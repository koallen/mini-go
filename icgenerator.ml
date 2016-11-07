open Ast

type irc = IRC of (irc_proc list)

and irc_cmd = IRC_Assign of string * irc_exp
            | IRC_Label of int
            | IRC_Goto of int
            | IRC_NonzeroJump of string * int  (* if x L = if x non-zero then jump to L *)
            | IRC_ZeroJump of string * int
            | IRC_Param of string
            | IRC_Call of int * int (* (label, number of parameters *)
            | IRC_Print of irc_exp
            | IRC_Return of string
            | IRC_Get of string

and irc_exp = IRC_Eq of string * string
            | IRC_Gt of string * string
            | IRC_Plus of string * string
            | IRC_Minus of string * string
            | IRC_Times of string * string
            | IRC_Division of string * string
            | IRC_IConst of int
            | IRC_Var of string

and irc_proc = IRC_Proc of irc_cmd list * string list

and environ = Environ of (environ option * (string * string) list)

let rec prettyPrint cmd = match cmd with
  | x::xs -> Printf.printf "%s " x; prettyPrint xs
  | [] -> Printf.printf "\n"

(* environment lookup *)
let rec lookup el lst = match lst with
  | Environ (parentEnviron, currentEnviron) -> (try (snd (List.find (fun (el2,_) -> el = el2) currentEnviron)) with
                                        | Not_found -> (match parentEnviron with
                                                        | None -> failwith "variable not declared: " ^ el
                                                        | Some env -> lookup el env))

let lookupCurrentScope el lst = match lst with
  | Environ (_, currentEnviron) -> (try (Some (snd (List.find (fun (el2,_) -> el = el2) currentEnviron))) with
                            | Not_found -> None)

let extendEnv env var1 type1 = match env with
  | Environ (parentEnviron, currentEnviron) -> let result = lookupCurrentScope var1 env in
                                       match result with
                                       | Some _ -> failwith "redeclaration"
                                       | None -> Environ (parentEnviron, (var1, type1)::currentEnviron)

let nameSupply = ref 1
let freshName _ =  nameSupply := !nameSupply + 1;
                   String.concat "" ["temp" ; string_of_int (!nameSupply )]

let varSupply = ref 1
let freshVar _ =  nameSupply := !nameSupply + 1;
                   String.concat "" ["var" ; string_of_int (!nameSupply )]

let labelSupply = ref 1
let freshLabel _ =  labelSupply := !labelSupply + 1;
                    !labelSupply

(* (parts) of translation of Booleans (short-circuit evaluation!),
   yields a tuple where first component represents the IRC and
   second component a variable name which is bound to the result *)
let rec translateB exp env = match exp with
  | BConst true -> let x = freshName() in
                   ([IRC_Assign (x, IRC_IConst 1)], x)
  | BConst false -> let x = freshName() in
                    ([IRC_Assign (x, IRC_IConst 0)], x)
  | And (e1, e2) -> (*
                       e1.code;
                       if !e1.result goto l1
                       e2.code;
                       x = e2.result;
                       goto l2;
                       l1:
                       x = 0;             Booleans represented as integers
                       l2:
                     *)
                    let r1 = translateExp e1 env in
                    let r2 = translateExp e2 env in
                    let x = freshName() in
                    let l1 = freshLabel() in
                    let l2 = freshLabel() in
                    ((fst r1)
                     @
                     [IRC_ZeroJump (snd r1,l1)]
                     @
                     (fst r2)
                     @
                     [IRC_Assign (x, IRC_Var (snd r2));
                      IRC_Goto l2 ]
                     @
                     [IRC_Label l1;
                      IRC_Assign (x, IRC_IConst 0);
                      IRC_Label l2],
                     x)
  | Not exp -> let r1 = translateExp exp env in
               let x = freshName() in
               let y = freshName() in
               ((fst r1)
                @
                [IRC_Assign (y, IRC_IConst 1);
                 IRC_Assign (x, IRC_Minus (y, (snd r1)))],
                x)

and translateExp exp env = match exp with
  | Plus (e1, e2) -> let r1 = translateExp e1 env in
                     let r2 = translateExp e2 env in
                     let x = freshName() in
                     ((fst r1)
                      @
                      (fst r2)
                      @
                      [IRC_Assign (x, IRC_Plus (snd r1, snd r2))],
                      x)
  | Minus (e1, e2) -> let r1 = translateExp e1 env in
                     let r2 = translateExp e2 env in
                     let x = freshName() in
                     ((fst r1)
                      @
                      (fst r2)
                      @
                      [IRC_Assign (x, IRC_Minus (snd r1, snd r2))],
                      x)
  | Times (e1, e2) -> let r1 = translateExp e1 env in
                     let r2 = translateExp e2 env in
                     let x = freshName() in
                     ((fst r1)
                      @
                      (fst r2)
                      @
                      [IRC_Assign (x, IRC_Times (snd r1, snd r2))],
                      x)
  | Division (e1, e2) -> let r1 = translateExp e1 env in
                     let r2 = translateExp e2 env in
                     let x = freshName() in
                     ((fst r1)
                      @
                      (fst r2)
                      @
                      [IRC_Assign (x, IRC_Division (snd r1, snd r2))],
                      x)
  | IConst integer -> let x = freshName() in
                      ([IRC_Assign (x, IRC_IConst integer)],
                       x)
  | Var varName -> let x = lookup varName env in
                   (*Printf.printf "%s" x;*)
                   ([], x)
  | Eq (exp1, exp2) -> let r1 = translateExp exp1 env in
                       let r2 = translateExp exp2 env in
                       let x = freshName() in
                       ((fst r1)
                        @
                        (fst r2)
                        @
                        [IRC_Assign (x, IRC_Eq (snd r1, snd r2))],
                        x)
  | Gt (exp1, exp2) -> let r1 = translateExp exp1 env in
                       let r2 = translateExp exp2 env in
                       let x = freshName() in
                       ((fst r1)
                        @
                        (fst r2)
                        @
                        [IRC_Assign (x, IRC_Gt (snd r1, snd r2))],
                        x)
  | _ -> translateB exp env

let rec translateStmt stmt env locals = match stmt with
  | Seq (stmt1, stmt2) -> let r1 = translateStmt stmt1 env locals in
                          let r2 = translateStmt stmt2 (fst (snd r1)) (snd (snd r1)) in
                          (*prettyPrint locals;*)
                          (*prettyPrint (snd (snd r2));*)
                          ((fst r1)
                           @
                           (fst r2),
                           (snd r2)
                           )
  | Decl (var, exp) -> let r1 = translateExp exp env in
                       let x = freshVar() in
                       let newEnv = extendEnv env var x in
                       let newLocal = x::locals in
                       ((fst r1)
                        @
                        [IRC_Assign (x, IRC_Var (snd r1))],
                        (newEnv,
                        newLocal))
  | Assign (var, exp) -> let r1 = translateExp exp env in
                         let x = lookup var env in
                         ((fst r1)
                          @
                          [IRC_Assign (x, IRC_Var (snd r1))],
                          (env,
                          locals))
  | Print exp -> let r1 = translateExp exp env in
                 ((fst r1)
                  @
                  [IRC_Print (IRC_Var (snd r1))],
                  (env, locals))
  | While (exp1, stmt1) -> let l1 = freshLabel() in
                           let l2 = freshLabel() in
                           let r1 = translateExp exp1 env in
                           let r2 = translateStmt stmt1 (Environ (Some env, [])) locals in
                           Printf.printf "%s" (snd r1);
                           ([IRC_Label l1]
                            @
                            (fst r1)
                            @
                            [IRC_ZeroJump ((snd r1), l2)]
                            @
                            (fst r2)
                            @
                            [IRC_Goto l1]
                            @
                            [IRC_Label l2],
                            (env, locals))
  | ITE (exp1, stmt1, stmt2) -> let l1 = freshLabel() in
                                let l2 = freshLabel() in
                                let r1 = translateExp exp1 env in
                                let r2 = translateStmt stmt1 (Environ (Some env, [])) locals in
                                let r3 = translateStmt stmt2 (Environ (Some env, [])) locals in
                                ((fst r1)
                                 @
                                 [IRC_ZeroJump ((snd r1), l2)]
                                 @
                                 (fst r2)
                                 @
                                 [IRC_Goto l1; IRC_Label l2]
                                 @
                                 (fst r3)
                                 @
                                 [IRC_Label l1],
                                 (env, locals))
  | FuncCall (funcName, args) -> let r1 = List.map (fun x -> translateExp x env) args in
                                 let r2 = List.concat (List.map fst r1) in
                                 let r3 = List.map (fun x -> IRC_Param (snd x)) r1 in
                                 let l1 = int_of_string (lookup funcName env) in
                                 (r2
                                  @
                                  r3
                                  @
                                  [IRC_Call (l1, List.length r1)],
                                  (env, locals))

let rec collectLabels procs labels = match procs with
  | x::xs -> (match x with
              | Proc (name, _, _, _) -> let l1 = freshLabel() in
                                        collectLabels xs ((name, string_of_int l1)::labels))
  | [] -> Environ(None, labels)

let rec translateProc procs env irc_procs = match procs with
  | x::xs -> (match x with
              | Proc (name, params, retTy, body) -> let r1 = translateStmt body env (List.rev (List.map (fun x -> match x with
                                                                                                                 | (Var name, _) -> name) params)) in
                                                    let l1 = lookup name env in
                                                    let irc_proc = IRC_Proc ((fst r1), (snd (snd r1))) in
                                                    translateProc xs env irc_procs@[irc_proc])
  | [] -> irc_procs

let translateProg prog = match prog with
  | Prog (procs, main) -> let funcLabels = collectLabels procs [] in
                          let translatedProcs = translateProc procs funcLabels [] in
                          let translatedMain = translateStmt main funcLabels [] in
                          IRC (translatedProcs @ [IRC_Proc ((fst translatedMain), (snd (snd translatedMain)))])

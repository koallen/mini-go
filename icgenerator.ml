open Ast

type irc = IRC of (irc_cmd list)

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

and irc_exp = IRC_And of string * string
            | IRC_Eq of string * string
            | IRC_Gt of string * string
            | IRC_Plus of string * string
            | IRC_Minus of string * string
            | IRC_Times of string * string
            | IRC_Division of string * string
            | IRC_Not of string
            | IRC_IConst of int
            | IRC_Var of string

and environ = Environ of (environ option * (string * string) list)

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

let labelSupply = ref 1
let freshLabel _ =  labelSupply := !labelSupply + 1;
                    !labelSupply

(* short-hand for 'zero' jump *)
let irc_ZeroJump (x,l) = let y = freshName() in
                         [IRC_Assign (y, IRC_Not x);
                          IRC_NonzeroJump (y,l)]


(* (parts) of translation of Booleans (short-circuit evaluation!),
   yields a tuple where first component represents the IRC and
   second component a variable name which is bound to the result *)
let rec translateB exp = match exp with
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
                    let r1 = translateB e1 in
                    let r2 = translateB e2 in
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
  (*| Not exp -> let r1 = translateB exp in*)
               (*let x = freshName() in*)
               (*let *)

let rec translateExp exp env = match exp with
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
                   ([], x)
  | _ -> translateB exp

let rec translateStmt stmt env = match stmt with
  | Seq (stmt1, stmt2) -> let r1 = translateStmt stmt1 env in
                          let r2 = translateStmt stmt2 (snd r1) in
                          ((fst r1)
                           @
                           (fst r2),
                           (snd r2))
  | Decl (var, exp) -> let r1 = translateExp exp env in
                       let x = freshName() in
                       let newEnv = extendEnv env var x in
                       ((fst r1)
                        @
                        [IRC_Assign (x, IRC_Var (snd r1))],
                        newEnv)
  | Assign (var, exp) -> let r1 = translateExp exp env in
                         let x = lookup var env in
                         ((fst r1)
                          @
                          [IRC_Assign (x, IRC_Var (snd r1))],
                          env)
  | Print exp -> let r1 = translateExp exp env in
                 ((fst r1)
                  @
                  [IRC_Print (IRC_Var (snd r1))],
                  env)
  | While (exp1, stmt1) -> let l1 = freshLabel() in
                           let l2 = freshLabel() in
                           let r1 = translateExp exp1 env in
                           let r2 = translateStmt stmt1 (Environ (Some env, [])) in
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
                            env)
  | ITE (exp1, stmt1, stmt2) -> let l1 = freshLabel() in
                                let l2 = freshLabel() in
                                let r1 = translateExp exp1 env in
                                let r2 = translateStmt stmt1 (Environ (Some env, [])) in
                                let r3 = translateStmt stmt2 (Environ (Some env, [])) in
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
                                 env)

let translateProg prog env = match prog with
  | Prog (procs, main) -> translateStmt main env

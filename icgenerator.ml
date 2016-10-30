open Ast

type irc = IRC of (irc_cmd list)

and irc_cmd = IRC_Assign of string * irc_exp
            | IRC_Label of int
            | IRC_Goto of int
            | IRC_NonzeroJump of string * int  (* if x L = if x non-zero then jump to L *)
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
                     (irc_ZeroJump (snd r1,l1))
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

let rec translateStmt stmt env = match stmt with
  | Seq (stmt1, stmt2) -> let r1 = translateStmt stmt1 in
                          let r2 = translateStmt stmt2 in
                          ((fst r1)
                           @
                           (fst r2),
                           (snd r2))
  | Decl (var, exp) -> let r1 = translateExp exp env in
                       let x = freshName() in
                       ((fst r1)
                        @
                        [IRC_Assign (x, IRC_Var (snd r1))],
                        x)
  | Assign (var, exp) -> let r1 = translateExp exp env in
                         let x = freshName() in
                         ((fst r1)
                          @
                          [IRC_Assign (x, IRC_Var (snd r1))],
                          x)
  | Print exp -> let r1 = translateExp exp env in
                 ((fst r1)
                  @
                  [IRC_Print (IRC_Var (snd r1))],
                  (snd r1))

let translateProg prog env = match prog with
  | Prog (procs, main) -> translateStmt main env


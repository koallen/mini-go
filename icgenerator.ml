open Ast

type irc = IRC of (irc_cmd list)

and irc_cmd = IRC_Assign of string * irc_exp
            | IRC_Label of int
            | IRC_Goto of int
            | IRC_NonzeroJump of string * int  (* if x L = if x non-zero then jump to L *)
            | IRC_Param of string
            | IRC_Call of int * int (* (label, number of parameters *)
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

(* short-hand for 'zero' jump *)
let irc_ZeroJump (x,l) = let y = freshName() in
                         [IRC_Assign (y, IRC_Not x);
                          IRC_NonzeroJump (y,l)]

let nameSupply = ref 1
let freshName _ =  nameSupply := !nameSupply + 1;
                   String.concat "" ["temp" ; string_of_int (!nameSupply )] 

let labelSupply = ref 1
let freshLabel _ =  labelSupply := !labelSupply + 1;
                    !labelSupply


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

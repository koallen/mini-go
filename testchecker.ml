#load "ast.cmo";;
#use "checker.ml";;

(* Test type checking *)
let env = [("max", (TyFunc([TyInt; TyInt], Some TyBool)))];;
let funcexp = FuncExp ("max", [IConst 1; IConst 9]);;
Printf.printf "\n\n------------------------TEST-----------------------\n";;
Printf.printf "The type of %s is: %s\n" (string_of_exp funcexp) (string_of_type (getType (inferTyExp env funcexp)));;

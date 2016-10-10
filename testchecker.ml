#load "ast.cmo";;
#use "checker.ml";;

(* Test type checking *)
let env = [("max", (TyFunc([TyInt; TyInt], TyInt)))];;
let funcexp = FuncExp ("max", [IConst 1; IConst 9]);;
inferTyExp env funcexp;;

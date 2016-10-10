open Ast

(* equality among types *)
let rec eqTy t1 t2 = match (t1,t2) with
  | (TyInt, TyInt) -> true
  | (TyBool, TyBool) -> true
  | (TyChan t1, TyChan t2) -> eqTy t1 t2
  | (TyFunc (ts1, t1), TyFunc (ts2, t2)) -> eqTy t1 t2 &&
                                            (List.length ts1 == List.length ts2) &&
                                            (List.for_all (fun (t1,t2) -> eqTy t1 t2) (List.combine ts1 ts2))
  | _ -> false  

(* environment lookup *)
let lookup el lst = try (Some (snd (List.find (fun (el2,_) -> el = el2) lst))) with
                    | Not_found -> None

let getType somety = match somety with
                     | Some x -> x
                     

(* type checking expressions *)
let rec inferTyExp env e = match e with
  | IConst i -> Some TyInt
  | BConst b -> Some TyBool
  | Var var  -> lookup var env
  | And (exp1,exp2) -> let type1= inferTyExp env exp1 in
                       let type2= inferTyExp env exp2 in 
                       (match type1 with
                       | Some x -> if (eqTy x TyBool) then 
                                   (match type2 with 
                                    | Some y -> if (eqTy y x) then Some TyBool
					else None
				    | None -> None)
                                   else None
                       | None -> None)
  | Eq (exp1,exp2) -> let type1= inferTyExp env exp1 in
                      let type2= inferTyExp env exp2 in
                      (match type1 with 
                      | Some x -> (match type2 with
                                   | Some y -> if(eqTy x y) then Some TyBool
                                               else None
                                   | None -> None)
                      | None -> None)
  | Gt (exp1,exp2) -> let type1= inferTyExp env exp1 in
                      let type2= inferTyExp env exp2 in
                      (match type1 with 
                      | Some x -> (match type2 with
                                   | Some y -> if(eqTy x y) then Some TyBool
                                               else None
                                   | None -> None)
                      | None -> None)
  | Plus (exp1,exp2) -> let type1= inferTyExp env exp1 in
                       let type2= inferTyExp env exp2 in 
                       (match type1 with
                       | Some x -> if (eqTy x TyInt) then 
                                   (match type2 with 
                                    | Some y -> if (eqTy y x) then Some TyInt
					else None
				    | None -> None)
                                   else None
                       | None -> None)
  | Minus (exp1,exp2) -> let type1= inferTyExp env exp1 in
                       let type2= inferTyExp env exp2 in 
                       (match type1 with
                       | Some x -> if (eqTy x TyInt) then 
                                   (match type2 with 
                                    | Some y -> if (eqTy y x) then Some TyInt
					else None
				    | None -> None)
                                   else None
                       | None -> None)
  | Times (exp1,exp2) -> let type1= inferTyExp env exp1 in
                       let type2= inferTyExp env exp2 in 
                       (match type1 with
                       | Some x -> if (eqTy x TyInt) then 
                                   (match type2 with 
                                    | Some y -> if (eqTy y x) then Some TyInt
					else None
				    | None -> None)
                                   else None
                       | None -> None)
  | Division (exp1,exp2) -> let type1= inferTyExp env exp1 in
                       let type2= inferTyExp env exp2 in 
                       (match type1 with
                       | Some x -> if (eqTy x TyInt) then 
                                   (match type2 with 
                                    | Some y -> if (eqTy y x) then Some TyInt
					else None
				    | None -> None)
                                   else None
                       | None -> None)
  | Not exp1 -> let type1= inferTyExp env exp1 in 
                (match type1 with
                | Some x -> if (eqTy x TyBool) then Some TyBool 
                            else None
                | None -> None)
  | RcvExp var1 -> let type1= lookup var1 env in
                   (match type1 with
                   | Some x -> if (eqTy x (TyChan TyInt)) then Some TyInt
                               else None
                   | None -> None)
  | FuncExp (name, args) -> let funTy= lookup name env in
                            (match funTy with 
              	             | Some func -> (match func with
                                             | TyFunc(paramsTy, retTy) -> let argTypes = List.map (fun x -> (inferTyExp env x)) args in
                                                                          if (List.exists (fun x -> match x with
                                                                                                    | None -> true
                                                                                                    | _ -> false) argTypes) then None
                                                                          else if (eqTy func (TyFunc(List.map getType argTypes, retTy))) then Some retTy
									  else None
                                             | _ -> None )
                             | None -> None )
  | _ -> None

			    
(* type checking statements *)
let rec typeCheckStmt env stmt = match stmt with
  | Assign (v,e) -> match (lookup v env) with
                    | None -> None (* Unknown variable *)
                    | Some t1 -> let t2 = inferTyExp env e in
                                 match t2 with
                                 | None -> None
                                 | Some t3 -> if eqTy t1 t3
                                              then Some env
                                              else None


(*
What's still missing are implementations for
    (1) collection of type signatures from functions (procedures)
    (2) type checking of procedure bodies, and
    (3) type checking of the main program.
*)

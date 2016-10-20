open Ast

(* equality among types *)
let rec eqTy t1 t2 = match (t1,t2) with
  | (TyInt, TyInt) -> true
  | (TyBool, TyBool) -> true
  | (TyChan t1, TyChan t2) -> eqTy t1 t2
  | (TyFunc (ts1, Some t1), TyFunc (ts2, Some t2)) -> eqTy t1 t2 &&
                                                      (List.length ts1 == List.length ts2) &&
                                                      (List.for_all (fun (t1,t2) -> eqTy t1 t2) (List.combine ts1 ts2))
  | (TyFunc (ts1, None), TyFunc (ts2, None)) -> (List.length ts1 == List.length ts2) &&
                                                (List.for_all (fun (t1,t2) -> eqTy t1 t2) (List.combine ts1 ts2))
  | _ -> false

(* environment lookup *)
let lookup el lst = try (Some (snd (List.find (fun (el2,_) -> el = el2) lst))) with
                    | Not_found -> None

let getType somety = match somety with
                     | Some x -> x
                     | None -> failwith "runtime error"

let extendEnv env var1 type1 =
    let result = lookup var1 env in
    match result with
    | Some _ -> let envWithoutVar1 = List.filter (fun x -> match x with
                                                  | (var2, _) -> if var1 = var2 then false else true) env in
                ((var1, type1)::envWithoutVar1)
    | None -> ((var1, type1)::env)

(* type checking expressions *)
let rec inferTyExp env e = match e with
  | IConst i -> Some TyInt
  | BConst b -> Some TyBool
  | Var var  -> lookup var env
  | And (exp1,exp2) -> let type1= inferTyExp env exp1 in
                       let type2= inferTyExp env exp2 in
                       (match type1 with
                        | Some x -> if (eqTy x TyBool) then (match type2 with
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
                         | Some x -> if (eqTy x TyInt) then (match type2 with
                                                             | Some y -> if (eqTy y x) then Some TyInt
                                                                         else None
                                                             | None -> None)
                                     else None
                         | None -> None)
  | Minus (exp1,exp2) -> let type1= inferTyExp env exp1 in
                         let type2= inferTyExp env exp2 in
                         (match type1 with
                          | Some x -> if (eqTy x TyInt) then (match type2 with
                                                              | Some y -> if (eqTy y x) then Some TyInt
                                                                          else None
                                                              | None -> None)
                                      else None
                          | None -> None)
  | Times (exp1,exp2) -> let type1= inferTyExp env exp1 in
                         let type2= inferTyExp env exp2 in
                         (match type1 with
                          | Some x -> if (eqTy x TyInt) then (match type2 with
                                                              | Some y -> if (eqTy y x) then Some TyInt
                                                                          else None
                                                              | None -> None)
                                      else None
                          | None -> None)
  | Division (exp1,exp2) -> let type1= inferTyExp env exp1 in
                            let type2= inferTyExp env exp2 in
                            (match type1 with
                             | Some x -> if (eqTy x TyInt) then (match type2 with
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
                                             | TyFunc(paramsTy, Some retTy) -> let argTypes = List.map (fun x -> (inferTyExp env x)) args in
                                                                               if List.mem None argTypes then None
                                                                               else if (eqTy func (TyFunc(List.map getType argTypes, Some retTy))) then Some retTy
                                                                               else None
                                             | _ -> None )
                             | None -> None )

(* type checking statements *)
let rec typeCheckStmt env stmt funcName = match stmt with
  | Assign (v,e) -> (match (lookup v env) with
                     | None -> None (* Unknown variable *)
                     | Some t1 -> let t2 = inferTyExp env e in
                                  match t2 with
                                  | None -> None
                                  | Some t3 -> if eqTy t1 t3 then Some env
                                               else None)
  | Seq (stmt1, stmt2) -> let g1 = typeCheckStmt env stmt1 funcName in
                          (match g1 with
                           | None -> None
                           | Some env1 -> let g2 = typeCheckStmt env1 stmt2 funcName in
                                          (match g2 with
                                           | None -> None
                                           | Some env2 -> Some env2))
  | Go stmt1 -> let g1 = typeCheckStmt env stmt1 funcName in
                (match g1 with
                 | None -> None
                 | Some _ -> Some env)
  | Transmit (chan, exp1) -> let type1 = inferTyExp env exp1 in
                             (match type1 with
                              | Some TyInt -> let type2 = lookup chan env in
                                               (match type2 with
                                                | Some (TyChan TyInt) -> Some env
                                                | _ -> None)
                              | _ -> None)
  | RcvStmt chan -> let type1 = lookup chan env in
                    (match type1 with
                     | Some (TyChan TyInt) -> Some env
                     | _ -> None)
  | Decl (var, exp1) -> let type1 = inferTyExp env exp1 in
                        (match type1 with
                         | Some x -> Some (extendEnv env var x)
                         | None -> None)
  | DeclChan chan -> Some (extendEnv env chan (TyChan TyInt))
  | While (exp1, stmt1) -> let type1 = inferTyExp env exp1 in
                           (match type1 with
                            | Some TyBool -> let g2 = typeCheckStmt env stmt1 funcName in
                                             (match g2 with
                                              | Some env2 -> Some env
                                              | None -> None)
                            | _ -> None)
  | ITE (exp1, stmt1, stmt2) -> let type1 = inferTyExp env exp1 in
                                (match type1 with
                                 | Some TyBool -> let g1 = typeCheckStmt env stmt1 funcName in
                                                  (match g1 with
                                                   | Some env1 -> let g2 = typeCheckStmt env stmt2 funcName in
                                                                  (match g2 with
                                                                   | Some env2 -> Some env
                                                                   | None -> None)
                                                   | None -> None)
                                 | _ -> None)
  | Return exp1 -> let type1 = inferTyExp env exp1 in
                   (match type1 with
                    | Some x -> let funcDef = lookup funcName env in
                                (match funcDef with
                                 | Some (TyFunc (_, Some returnType)) -> if eqTy x returnType then Some env
                                                                         else None
                                 | _ -> None)
                    | None -> None)
  | FuncCall (name, args) -> let funcDef = lookup name env in
                             (match funcDef with
                              | Some (TyFunc (params, retTy)) -> let argTypes = List.map (fun x -> (inferTyExp env x)) args in
                                                                 if List.mem None argTypes then None
                                                                 else if (eqTy (TyFunc (params, retTy)) (TyFunc(List.map getType argTypes, retTy))) then Some env
                                                                 else None
                              | _ -> None)
  | Print exp1 -> let type1 = inferTyExp env exp1 in
                  (match type1 with
                   | Some x -> Some env
                   | None -> None)
  | Skip -> None

let rec collectFuncs procs env = match procs with
  | proc::remainingProcs -> (match proc with
                             | Proc (name, [], None, _) -> let newEnv = extendEnv env name (TyFunc([], None)) in
                                                           collectFuncs remainingProcs newEnv
                             | Proc (name, [], Some retTy, _) -> let newEnv = extendEnv env name (TyFunc([], Some retTy)) in
                                                                 collectFuncs remainingProcs newEnv
                             | Proc (name, params, None, _) -> let newEnv = extendEnv env name (TyFunc(List.map (fun x -> match x with
                                                                                                                          | (exp1, type1) -> type1)
                                                                                                                params, None)) in
                                                               collectFuncs remainingProcs newEnv
                             | Proc (name, params, Some retTy, _) -> let newEnv = extendEnv env name (TyFunc(List.map (fun x -> match x with
                                                                                                                                | (exp1, type1) -> type1)
                                                                                                                      params, Some retTy)) in
                                                                     collectFuncs remainingProcs newEnv)
   | [] -> env

let rec addParamsToEnv params env = match params with
  | param::remainingParams -> (match param with
                               | (Var var1, type1) -> let newEnv = extendEnv env var1 type1 in
                                                      addParamsToEnv remainingParams newEnv
                               | _ -> failwith "runtime error")
  | [] -> env

let rec checkReturnStatement body = match body with
  | Seq (_, remainingStatements) -> checkReturnStatement remainingStatements
  | Return _ -> true
  | _ -> false

let rec typeCheckFunctions procs env = match procs with
  | proc::remainingProcs -> (match proc with
                             | Proc (name, params, None, body) -> let envWithParam = addParamsToEnv params env in
                                                                  let checkedEnv = typeCheckStmt envWithParam body name in
                                                                  (match checkedEnv with
                                                                   | Some _ -> typeCheckFunctions remainingProcs env
                                                                   | None -> false)
                             | Proc (name, params, Some _, body) -> if (not (checkReturnStatement body)) then
                                                                        false
                                                                    else begin
                                                                        let envWithParam = addParamsToEnv params env in
                                                                        let checkedEnv = typeCheckStmt envWithParam body name in
                                                                        (match checkedEnv with
                                                                         | Some _ -> typeCheckFunctions remainingProcs env
                                                                         | None -> false)
                                                                    end)
   | [] -> true

let typeCheckProgram prog = match prog with
  | Prog (procs, main) -> let envWithFunc = collectFuncs procs [] in
                          if typeCheckFunctions procs envWithFunc
                          then let typeCheckMain = typeCheckStmt envWithFunc main "" in
                               (match typeCheckMain with
                                | Some _ -> true
                                | None -> false)
                          else false

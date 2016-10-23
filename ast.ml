type prog = Prog of (proc list) * stmt

and proc = Proc of string * ((exp * types) list) * (types option) * stmt

and types = TyInt
           | TyBool
           | TyChan of types
           | TyFunc of (types list * types option)

and stmt = Seq of stmt * stmt
          | Go of stmt
          | Transmit of string * exp
          | RcvStmt of string 
          | Decl of string * exp
          | DeclChan of string
          | Assign of string * exp
          | While of exp * stmt
          | ITE of exp * stmt * stmt
          | Return of exp
          | FuncCall of string * (exp list)
          | Print of exp
          | Skip

and exp = And of exp * exp
         | Eq of exp * exp
         | Gt of exp * exp
         | Plus of exp * exp
         | Minus of exp * exp
         | Times of exp * exp
         | Division of exp * exp
         | Not of exp
         | RcvExp of string
         | IConst of int
         | BConst of bool
         | Var of string
         | FuncExp of string * (exp list)

and environ = Environ of (environ option * (string * types) list)

let rec string_of_exp exp = match exp with
    | And (exp1, exp2)       -> "And (" ^ (string_of_exp exp1) ^ ", " ^ (string_of_exp exp2) ^ ")"
    | Eq (exp1, exp2)        -> "Eq (" ^ (string_of_exp exp1) ^ ", " ^ (string_of_exp exp2) ^ ")"
    | Gt (exp1, exp2)        -> "Gt (" ^ (string_of_exp exp1) ^ ", " ^ (string_of_exp exp2) ^ ")"
    | Plus (exp1, exp2)      -> "Plus (" ^ (string_of_exp exp1) ^ ", " ^ (string_of_exp exp2) ^ ")"
    | Minus (exp1, exp2)     -> "Minus (" ^ (string_of_exp exp1) ^ ", " ^ (string_of_exp exp2) ^ ")"
    | Times (exp1, exp2)     -> "Times (" ^ (string_of_exp exp1) ^ ", " ^ (string_of_exp exp2) ^ ")"
    | Division (exp1, exp2)  -> "Division (" ^ (string_of_exp exp1) ^ ", " ^ (string_of_exp exp2) ^ ")"
    | Not exp                -> "Not (" ^ (string_of_exp exp) ^ ")"
    | RcvExp str             -> "RcvExp " ^ str
    | IConst integer         -> "IConst " ^ string_of_int integer
    | BConst boolean         -> "BConst " ^ string_of_bool boolean
    | Var str                -> "Var " ^ str
    | FuncExp (str, exps)    -> "FuncExp (" ^ str ^ ", [" ^ "list of exps" ^ "])"

let string_of_type ty = match ty with
    | TyInt        -> "TyInt"
    | TyBool       -> "TyBool"
    | TyChan TyInt -> "TyChan TyInt"
    | _            -> "TyFunc"

let rec string_of_env env = match env with
    | binding1::remainingBindings -> (match binding1 with
                                     | (var1, type1) -> var1 ^ ", " ^ (string_of_type type1) ^ "\n" ^ (string_of_env remainingBindings))
    | [] -> ""


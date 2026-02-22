(* Define : <- *)
(* Seq : ; *)
(* Define : repeat m do m od*)

(**x<-5;y<-37;z<-(x+y)*)


(**            
                    SEQ
                     /\
                    /  \
               Define x SEQ
                 |       /\
              CST "5"   /  \
                  Define y  Define z
                       |       |
                    CST "37"  Plus
                              /   \
                           Var x  Var y 


*)

(** On pourrais écrire 
type op_cmp = 
    | Eq
    | gt  
    
un truc commme ça 

Une expression pour dire qu'une variable est inférier ou égale 0 ?

non(x > 0) l'inverse de > donne (< et =)

ce qui donne Opun(Non, Opcmp(gt, Var "x", Cst 0))

*)

type est_log = 
    | Vrai
    | Faux

type op_un =
    | Non

type op_bin =
    | Plus
    | Moins
    | Fois
    | Div

type op_bin_log =
    | Et
    | Ou

type op_cmp =
    | Eq
    | Sup
    | Inf

type t_ident = string;;

type ea_var =
    | Cst of int
    | Var of t_ident
    | Opbin of op_bin * ea_var * ea_var

type t_exp_bcl =
    | Define of t_ident * ea_var
    | Sequence of t_exp_bcl * t_exp_bcl
    | Repeat of t_exp_bcl * ea_var

type expr_log_eq =
    | Cst of est_log
    | Opun of op_un * expr_log_eq
    | Opbin of op_bin_log * expr_log_eq * expr_log_eq
    | Opcmp of op_cmp * ea_var * ea_var

let string_of_cmp op =
    match op with
    | Eq -> "=="
    | Sup -> ">"
    | Inf -> "<"

let interp_opbin_log op b1 b2 =
    match op with
    | Et -> b1 && b2
    | Ou -> b1 || b2

let interp_opcmp op n1 n2 =
    match op with
    | Eq -> n1 = n2
    | Sup -> n1 > n2
    | Inf -> n1 < n2

let rec interp_ea_var (env : (t_ident * int) list) (e : ea_var) : int =
    match e with
    | Cst n -> n
    | Var id -> (try List.assoc id env with Not_found -> failwith ("Variable " ^ id ^ " non définie"))
    | Opbin (op, e1, e2) ->
        let n1 = interp_ea_var env e1 in
        let n2 = interp_ea_var env e2 in
        (match op with
        | Plus -> n1 + n2
        | Moins -> n1 - n2
        | Fois -> n1 * n2
        | Div -> n1 / n2)

let afficher_opbin (op : op_bin) : string =
    match op with
    | Plus -> "+"
    | Moins -> "-"
    | Fois -> "*"
    | Div -> "/"

let rec afficher_ea_var (e : ea_var) : string =
    match e with
    | Cst n -> string_of_int n
    | Var id -> id
    | Opbin (op, e1, e2) ->
        "(" ^ (afficher_ea_var e1) ^ " " ^ (afficher_opbin op) ^ " " ^ (afficher_ea_var e2) ^ ")"

let string_of_opbin_log op =
    match op with
    | Et -> " and "
    | Ou -> " or "

let rec string_of_op_expr_log_eq e =
    match e with
    | Cst Vrai -> "vrai"
    | Cst Faux -> "faux"
    | Opun(Non, e1) -> "non(" ^ (string_of_op_expr_log_eq e1) ^ ")"
    | Opbin(op, e1, e2) -> "(" ^ (string_of_op_expr_log_eq e1) ^ (string_of_opbin_log op) ^ (string_of_op_expr_log_eq e2) ^")"
    | Opcmp(op, e1, e2) -> "(" ^ (afficher_ea_var e1)  ^ (string_of_cmp op) ^ (afficher_ea_var e2) ^")"

let rec interp_expr_log_eq env e =
    match e with
    | Cst Vrai -> true
    | Cst Faux -> false
    | Opun(Non, e1) -> not(interp_expr_log_eq env e1)
    | Opbin(op, e1, e2) -> interp_opbin_log op (interp_expr_log_eq env e1) (interp_expr_log_eq env e2)
    | Opcmp(op, ea1, ea2) -> interp_opcmp op (interp_ea_var env ea1) (interp_ea_var env ea2)

let rec afficher_exp_bcl (exp : t_exp_bcl) : string =
    match exp with
    | Define (id, e) ->
        id ^ " <- " ^ (afficher_ea_var e)
    | Sequence (exp1, exp2) ->
        (afficher_exp_bcl exp1) ^ " ; " ^ (afficher_exp_bcl exp2)
    | Repeat (exp, e) ->
        "repeat " ^ (afficher_exp_bcl exp) ^ " do " ^ (afficher_ea_var e) ^ " od"


let x = Define ("x", Cst 5);;
let y = Define ("y", Cst 37);;
let z = Define ("z", Opbin (Plus, Var "x", Var "y"));;
let seq = Sequence (x, Sequence (y, z));;

let () = print_endline(afficher_exp_bcl seq)
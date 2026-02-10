(**Exercice 7 ) -5 *)

type est_log = 
    | Vrai
    | Faux

type expr_log = 
    | Cst of est_log
    | Opun of opun * expr_log
    | Opbin of opbin * expr_log * expr_log

let interp_est_log(c : est_log) : bool =
    match c with
    | Vrai -> true
    | Faux -> false

let rec interp_expr_log (e : expr_log) : bool =
    match e with
    | Cst c -> (interp_est_log c) (** Cst est une constante , ensuite je l'interprete avec la fonc interp_est log*)
    | Opun (Non, e1) -> not(interp_expr_log e1) (**de la partie non on passe a la partie interpéter en ocaml*)
    | Opbin (Et, e1, e2) -> (interp_expr_log e1) && (interp_expr_log e2) (**on fait le et entre e1 et e2**)
    | Opbin (Ou, e1, e2) -> (interp_expr_log e1) || (interp_expr_log e2) (**on fait le ou entre e1 et e2**)
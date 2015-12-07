(*
 * SNU 4190.310 Programming Languages 2015 Fall
 * Type Checker Skeleton
 * Jaeseung Choi (jschoi@ropas.snu.ac.kr)
 *)

open M

let count = ref 0 

let new_var () = 
  let _ = count := !count +1 in
  "x_" ^ (string_of_int !count)

type var = string
type typ = 
  | TInt
  | TBool
  | TString
  | TPair of typ * typ
  | TLoc of typ
  | TFun of typ * typ
  | TVar of var
  (* Modify, or add more if needed *)

type typ_scheme =
  | SimpleTyp of typ 
  | GenTyp of (var list * typ)

type typ_env = (M.id * typ_scheme) list
type subst = typ -> typ

let empty_subst : subst = fun t -> t

let make_subst : var -> typ -> subst = fun x t ->
  let rec subs t' = 
    match t' with
    | TVar x' -> if (x = x') then t else t'
    | TPair (t1, t2) -> TPair (subs t1, subs t2)
    | TLoc t'' -> TLoc (subs t'')
    | TFun (t1, t2) -> TFun (subs t1, subs t2)
    | _ -> t'
  in subs

(* Substitution Composition *)
let (@@) s1 s2 = (fun t -> s1 (s2 t))

let subst_scheme : subst -> typ_scheme -> typ_scheme = fun subs tscm ->
  match tscm with
  | SimpleTyp t -> SimpleTyp (subs t)
  | GenTyp (alphas, t) -> GenTyp (alphas, subs t)

let subst_env : subst -> typ_env -> typ_env = fun subs tenv ->
  List.map (fun (x, tscm) -> (x, subst_scheme subs tscm)) tenv

(* TODO : Implement this function *)
let check : M.exp -> M.typ =
  raise (M.TypeError "Type Checker Unimplemented")

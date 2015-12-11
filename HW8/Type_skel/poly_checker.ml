(*
 * SNU 4190.310 Programming Languages 2015 Fall
 * Type Checker Skeleton
 * Jaeseung Choi (jschoi@ropas.snu.ac.kr)
 *)

open M

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

let count = ref 0

let new_var () =
  let _ = count := !count +1 in
  "x_" ^ (string_of_int !count)

(* Get Type from Env *)
let get_type : typ_env -> M.id -> typ =
    fun env i ->
        if (List.exists (fun a -> (fst a) = i) env)
        then (snd (List.find (fun a -> (fst a) = i) env))
        else raise (M.TypeError "ID doesn't exist in Env")

(* Definitions related to free type variable *)

let union_ftv ftv_1 ftv_2 =
  let ftv_1' = List.filter (fun v -> not (List.mem v ftv_2)) ftv_1 in
  ftv_1' @ ftv_2

let sub_ftv ftv_1 ftv_2 =
  List.filter (fun v -> not (List.mem v ftv_2)) ftv_1

let rec ftv_of_typ : typ -> var list = function
  | TInt | TBool | TString -> []
  | TPair (t1, t2) -> union_ftv (ftv_of_typ t1) (ftv_of_typ t2)
  | TLoc t -> ftv_of_typ t
  | TFun (t1, t2) ->  union_ftv (ftv_of_typ t1) (ftv_of_typ t2)
  | TVar v -> [v]

let ftv_of_scheme : typ_scheme -> var list = function
  | SimpleTyp t -> ftv_of_typ t
  | GenTyp (alphas, t) -> sub_ftv (ftv_of_typ t) alphas

let ftv_of_env : typ_env -> var list = fun tyenv ->
  List.fold_left
    (fun acc_ftv (id, tyscm) -> union_ftv acc_ftv (ftv_of_scheme tyscm))
    [] tyenv

(* Generalize given type into a type scheme *)
let generalize : typ_env -> typ -> typ_scheme = fun tyenv t ->
  let env_ftv = ftv_of_env tyenv in
  let typ_ftv = ftv_of_typ t in
  let ftv = sub_ftv typ_ftv env_ftv in
  if List.length ftv = 0 then
    SimpleTyp t
  else
    GenTyp(ftv, t)

(* Definitions related to substitution *)

type subst = typ -> typ

let empty_subst : subst = fun t -> t

let make_subst : var -> typ -> subst = fun x t ->
  let rec subs t' =
    match t' with
    | TVar x' -> if (x = x') then t else t'
    | TPair (t1, t2) -> TPair (subs t1, subs t2)
    | TLoc t'' -> TLoc (subs t'')
    | TFun (t1, t2) -> TFun (subs t1, subs t2)
    | TInt | TBool | TString -> t'
  in subs

let (@@) s1 s2 = (fun t -> s1 (s2 t)) (* substitution composition *)

let subst_scheme : subst -> typ_scheme -> typ_scheme = fun subs tyscm ->
  match tyscm with
  | SimpleTyp t -> SimpleTyp (subs t)
  | GenTyp (alphas, t) ->
    (* S (\all a.t) = \all b.S{a->b}t  (where b is new variable) *)
    let betas = List.map (fun _ -> new_var()) alphas in
    let s' =
      List.fold_left2
        (fun acc_subst alpha beta -> make_subst alpha (TVar beta) @@ acc_subst)
        empty_subst alphas betas
    in
    GenTyp (betas, subs (s' t))

let subst_env : subst -> typ_env -> typ_env = fun subs tyenv ->
  List.map (fun (x, tyscm) -> (x, subst_scheme subs tyscm)) tyenv

(* Condition *)
type cond = var -> typ -> bool

let empty_cond : cond = fun v t -> true

let add_cond : cond -> var -> typ list -> cond = fun old v tl ->
    (fun v' t' ->
        if v = v'
        then (List.exists (fun x ->
                            match (x, t') with
                            | (TInt, TInt) -> true
                            | (TBool, TBool) -> true
                            | (TString, TString) -> true
                            | (TLoc _, TLoc _) -> true
                            | (_, TVar _) -> true
                            | _ -> false)
                            tl)
        else old v' t')


(* Unification *)
let rec exists : typ -> var -> bool =
    fun t v ->
        match t with
        | TVar (v') -> v = v'
        | TPair (t1, t2) -> (exists t1 v) || (exists t2 v)
        | TLoc t' -> exists t' v
        | TFun (t1, t2) -> (exists t1 v) || (exists t2 v)
        | _ -> false

let rec unify : typ -> typ -> subst =
    fun t1 t2 ->
        match (t1, t2) with
        | (TInt, TInt) -> empty_subst
        | (TBool, TBool) -> empty_subst
        | (TString, TString) -> empty_subst
        | (TVar a, t) -> if exists t a
                         then raise (M.TypeError "unify : t1 in t2")
                         else make_subst a t
        | (t, TVar a) -> if exists t a
                         then raise (M.TypeError "unify : t2 in t1")
                         else make_subst a t
        | (TPair (t1a, t1b), TPair (t2a, t2b)) ->
                let sa = unify t1a t2a in
                let sb = unify t1b t2b in
                (sa @@ sb)
        | (TLoc t1', TLoc t2') -> unify t1' t2'
        | (TFun (t1a, t1b), TFun (t2a, t2b)) ->
                let sa = unify t1a t2a in
                let sb = unify t1b t2b in
                (sa @@ sb)
        | _ -> raise (M.TypeError "unify : unhandled")

(* Expansive (about Memory) *)
let rec expansive : M.exp -> bool =
    fun e ->
        match e with
        | M.APP _ -> true
        | M.LET (d, e') ->
                (match d with
                 | M.REC (_, _, e'') -> expansive(e') || expansive(e'')
                 | M.VAL (_, e'') -> expansive(e') || expansive(e'')
                )
        | M.IF (e1, e2, e3) -> expansive(e1) || expansive(e2) || expansive(e3)
        | M.BOP (_ , e1, e2) -> expansive(e1) || expansive(e2)
        | M.WRITE e' -> expansive(e')
        | M.MALLOC _ -> true
        | M.ASSIGN (e1, e2) -> expansive(e1) || expansive(e2)
        | M.BANG e' -> expansive(e')
        | M.SEQ (e1, e2) -> expansive(e1) || expansive(e2)
        | M.PAIR (e1, e2) -> expansive(e1) || expansive(e2)
        | M.FST e' -> expansive(e')
        | M.SND e' -> expansive(e')
        | _ -> false

(* M with cond (check equal, write type) *)
let rec m : typ_env -> M.exp -> typ -> (subst * cond) =
    fun env e t ->
        match e with
        | M.CONST (S _) -> ((unify t TString), empty_cond)
        | M.CONST (N _) -> ((unify t TInt), empty_cond)
        | M.CONST (B _) -> ((unify t TBool), empty_cond)
        | M.VAR i ->
                (* env 없는 경우 get_type에서 예외 발생 *)
                ((unify t (get_type env i)), empty_cond)
        | M.FN (i, e') ->
                let v1 = new_var() in
                let v2 = new_var() in
                let s1 = unify t (TFun (TVar v1, TVar v2)) in
                let (s2, c) = m

(* TODO : Implement this function *)
let check : M.exp -> M.typ =
  raise (M.TypeError "Type Checker Unimplemented")

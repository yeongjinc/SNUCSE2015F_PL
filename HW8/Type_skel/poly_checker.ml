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


(* Get Type from Env *)
let get_type_scheme : typ_env -> M.id -> typ_scheme =
    fun env i ->
        if (List.exists (fun a -> (fst a) = i) env)
        then (snd (List.find (fun a -> (fst a) = i) env))
        else
            raise (M.TypeError "ID doesn't exist in Env")

(* Debug *)
(*
let prt : typ -> unit =
  let rec iter : typ -> unit =
    fun t ->
      match t with
      | TInt -> print_string "Int"
      | TBool -> print_string "Bool"
      | TString -> print_string "String"
      | TPair (t1, t2) ->
          let _ = print_string "(" in
          let _ = iter t1 in
          let _ = print_string ", " in
          let _ = iter t2 in
          print_string ")"
      | TFun (t1, t2) ->
          let _ = print_string "(" in
          let _ = iter t1 in
          let _ = print_string " → " in
          let _ = iter t2 in
          print_string ")"
      | TLoc t ->
          let _ = iter t in
          print_string "_Loc"
      | TVar v -> print_string ("Var " ^ v)
	in
  fun t ->
      let _ = iter t in
      print_string "\n"
*)

(* Condition *)
type cond = var -> typ -> bool

let empty_cond : cond = fun v t -> true

let print_type t =
    match t with
    | TInt -> print_endline "i"
    | TBool -> print_endline "b"
    | TString -> print_endline "s"
    | TLoc _ -> print_endline "l"
    | TVar _ -> print_endline "v"
    | TFun (_, _) -> print_endline "f"
    | TPair (_, _) -> print_endline "p"

let add_cond : cond -> var -> typ list -> cond = fun old v tl ->
    (fun v' t' ->
        if v = v'
        then
            (* let _ = print_type t' in
            let _ = print_endline "==" in
            let _ = List.iter print_type tl in *)
            (List.exists (fun x ->
                            match (x, t') with
                            | (TInt, TInt) -> true
                            | (TBool, TBool) -> true
                            | (TString, TString) -> true
                            | (TLoc _, TLoc _) -> true
                            | (_, TVar _) -> true
                            | _ -> false)
                            tl)
        else old v' t')

let append_cond : cond -> cond -> cond = fun c1 c2 ->
    (fun v t ->
        (c1 v t) && (c2 v t))

let rec check_cond : int -> cond -> subst -> bool =
    fun i c s ->
        let v = "x_" ^ (string_of_int i) in
        if i > !count
        then true
        else (c v (s (TVar v))) && (check_cond (i+1) c s)

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
        | (TVar a, t) -> if t1 = t2 then empty_subst
                         else if exists t a
                         then
                            raise (M.TypeError "unify : t1 in t2")
                         else make_subst a t
        | (t, TVar a) -> if t1 = t2 then empty_subst
                         else if exists t a
                         then
                            raise (M.TypeError "unify : t2 in t1")
                         else make_subst a t
        | (TPair (t1a, t1b), TPair (t2a, t2b)) ->
                let s1 = unify t1a t2a in
                let s2 = unify (s1 t1b) (s1 t2b) in
                (s2 @@ s1)
        | (TLoc t1', TLoc t2') -> unify t1' t2'
        | (TFun (t1a, t1b), TFun (t2a, t2b)) ->
                let s1 = unify t1a t2a in
                let s2 = unify (s1 t1b) (s1 t2b) in
                (s2 @@ s1)
        | _ ->
                (* let _ = prt t1 in
                let _ = prt t2 in *)
                raise (M.TypeError "unify : unhandled")

(* Expansive *)
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

let rec instantiate ts =
    match ts with
    | SimpleTyp t1 -> t1
    | GenTyp (vl, t1) ->
            let s = instantiate' vl empty_subst in
            s t1
and instantiate' vl t =
    match vl with
    | [] -> t
    | hd::tl ->
            let v = new_var() in
            let new_type = make_subst hd (TVar v) in
            (instantiate' tl new_type) @@ t

(* M with cond (check equal, write type) *)
let rec m : typ_env -> M.exp -> typ -> (subst * cond) =
    fun env e t ->
        match e with
        | M.CONST (M.S _) -> ((unify t TString), empty_cond)
        | M.CONST (M.N _) -> ((unify t TInt), empty_cond)
        | M.CONST (M.B _) -> ((unify t TBool), empty_cond)
        | M.VAR i ->
                (* env 없는 경우 get_type_scheme에서 예외 발생 *)
				let ts = get_type_scheme env i in
				let its = instantiate ts in
                ((unify t its), empty_cond)
        | M.FN (i, e1) ->
                let v1 = new_var() in
                let v2 = new_var() in
                let s1 = unify t (TFun (TVar v1, TVar v2)) in
                let env1 = [(i, SimpleTyp (s1 (TVar v1)))] @ (subst_env s1 env)  in
                let (s2, c) = m env1 e1 (s1 (TVar v2)) in
                ((s2 @@ s1), c)
        | M.APP (e1, e2) ->
                let v1 = new_var() in
                let (s1, c1) = m env e1 (TFun (TVar v1, t)) in
                let env1 = subst_env s1 env in
                let (s2, c2) = m env1 e2 (s1 (TVar v1)) in
                ((s2 @@ s1), append_cond c1 c2)
        | M.LET (d, e1) ->
                (match d with
                | M.REC (i1, i2, e2) ->
                        let v1 = new_var() in
                        let v2 = new_var() in
                        let rec_to_fun = TFun (TVar v1, TVar v2) in

                        (* let _ = if expansive e2 = true then print_endline "true" else print_endline "false" in
                           REC 의 e2는 함수본문 그 자체이므로 expansive에 넘겨줄 필요가 없다 *)
                        let f_t = generalize env rec_to_fun in
                        let env1 = [(i1, f_t)] @ env in
                        let (s1, c1) = m env1 (M.FN (i2, e2)) rec_to_fun in

                        let f_t2 = generalize (subst_env s1 env) (s1 rec_to_fun) in
                        (* env1이 아님! *)
                        let env2 = [(i1, f_t2)] @ (subst_env s1 env) in
                        let (s2, c2) = m env2 e1 (s1 t) in
                        ((s2 @@ s1), append_cond c1 c2)
                | M.VAL (i1, e2) ->
                        let v1 = new_var() in
                        let (s1, c1) = m env e2 (TVar v1) in
                        let env1 = subst_env s1 env in
                        let f_t = if (expansive e2 = true)
                                  then (SimpleTyp (s1 (TVar v1)))
                                  (* else (GenTyp ((ftv_of_typ (s1 (TVar v1))), (s1 (TVar v1)))) in *)
                                  else generalize env1 (s1 (TVar v1)) in
                        let env2 = env1 @ [(i1, f_t)] in
                        let (s2, c2) = m env2 e1 (s1 t) in
                        ((s2 @@ s1), append_cond c1 c2)
                )
        | M.IF (e1, e2, e3) ->
                let (s1, c1) = m env e1 TBool in
                let env1 = subst_env s1 env in
                let (s2, c2) = m env1 e2 (s1 t) in
                let env2 = subst_env s2 env1 in
                let (s3, c3) = m env2 e3 (s2 (s1 t)) in
                ((s3 @@ s2 @@ s1), append_cond (append_cond c1 c2) c3)
        | M.BOP (b, e1, e2) ->
                let v1 = new_var() in
                let op_t = match b with
                           | M.ADD -> TInt
                           | M.SUB -> TInt
                           | M.AND -> TBool
                           | M.OR -> TBool
                           | M.EQ -> TVar v1 in
                let res_t = match b with
                            | M.ADD -> TInt
                            | M.SUB -> TInt
                            | M.AND -> TBool
                            | M.OR -> TBool
                            | M.EQ -> TBool in
                let s1 = unify t res_t in
                let env1 = subst_env s1 env in
                let (s2, c1) = m env1 e1 (s1 op_t) in
                let env2 = subst_env s2 env1 in
                let (s3, c2) = m env2 e2 (s2 (s1 op_t)) in
                let s321 = s3 @@ s2 @@ s1 in
                (match b with
                | M.EQ ->
                        let v2 = new_var() in
                        let c3 = add_cond empty_cond v1 [TInt; TString; TBool; TLoc (TVar v2)] in
                        (s321, (append_cond (append_cond c1 c2) c3))
                | _ -> (s321, append_cond c1 c2)
                )
        | M.READ -> (unify t TInt, empty_cond)
        | M.WRITE e1 ->
                let v1 = new_var() in
                let s1 = unify t (TVar v1) in
                let env1 = subst_env s1 env in
                let (s2, c1) = m env1 e1 (s1 t) in
                let c2 = add_cond empty_cond v1 [TInt; TString; TBool] in
                (s2 @@ s1, append_cond c1 c2)
        | M.MALLOC e1 ->
                let v1 = new_var() in
                let s1 = unify t (TLoc (TVar v1)) in
                let env1 = subst_env s1 env in
                let (s2, c1) = m env1 e1 (s1 (TVar v1)) in
                (s2 @@ s1, c1)
        | M.ASSIGN (e1, e2) ->
                let (s1, c1) = m env e2 t in
                let env1 = subst_env s1 env in
                let (s2, c2) = m env1 e1 (s1 (TLoc t)) in
                (s2 @@ s1, append_cond c1 c2)
        | M.BANG e1 -> m env e1 (TLoc t)
        | M.SEQ (e1, e2) ->
                let v1 = new_var() in
                let (s1, c1) = m env e1 (TVar v1) in
                let env1 = subst_env s1 env in
                let (s2, c2) = m env1 e2 (s1 t) in
                (s2 @@ s1, append_cond c2 c1)
        | M.PAIR (e1, e2) ->
                let v1 = new_var() in
                let v2 = new_var() in
                let s1 = unify t (TPair (TVar v1, TVar v2)) in
                let env1 = subst_env s1 env in
                let (s2, c1) = m env1 e1 (s1 (TVar v1)) in
                let s21 = s2 @@ s1 in
                let env2 = subst_env s2 env1 in
                let (s3, c2) = m env2 e2 (s21 (TVar v2)) in
                (s3 @@ s21, append_cond c1 c2)
        | M.FST e1 ->
                let v1 = new_var() in
                m env e1 (TPair (t, (TVar v1)))
        | M.SND e1 ->
                let v1 = new_var() in
                m env e1 (TPair ((TVar v1), t))

let rec convert_to_mtyp : typ -> M.typ =
    fun t ->
        match t with
        | TInt -> M.TyInt
        | TBool -> M.TyBool
        | TString -> M.TyString
        | TPair (t1, t2) -> M.TyPair (convert_to_mtyp t1, convert_to_mtyp t2)
        | TLoc t1 -> M.TyLoc (convert_to_mtyp t1)
        | TFun (t1, t2) ->
                raise (M.TypeError "program is function only")
        | TVar v1 ->
                raise (M.TypeError "type var remains")

(* TODO : Implement this function *)
let check : M.exp -> M.typ =
    fun e ->
        let v = new_var() in
        (* let _ = print_endline v in *)
        let (s, c) = m [] e (TVar v) in
        if check_cond 0 c s
        then
            (* let _ = prt (s (TVar v)) in *)
            convert_to_mtyp (s (TVar v))
        else
            raise (M.TypeError "check_cond fail")


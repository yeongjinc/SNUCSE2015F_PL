(*
 * SNU 4190.310 Programming Languages 2015 Fall
 *  K- Interpreter Skeleton Code
 * Jaeseung Choi (jschoi@ropas.snu.ac.kr)
 *)

(* Location Signature *)
module type LOC =
sig
  type t
  val base : t
  val equal : t -> t -> bool
  val diff : t -> t -> int
  val increase : t -> int -> t
end

module Loc : LOC =
struct
  type t = Location of int
  let base = Location(0)
  let equal (Location(a)) (Location(b)) = (a = b)
  let diff (Location(a)) (Location(b)) = a - b
  let increase (Location(base)) n = Location(base+n)
end

(* Memory Signature *)
module type MEM =
sig
  type 'a t
  exception Not_allocated
  exception Not_initialized
  val empty : 'a t (* get empty memory *)
  val load : 'a t -> Loc.t  -> 'a (* load value : Mem.load mem loc => value *)
  val store : 'a t -> Loc.t -> 'a -> 'a t (* save value : Mem.store mem loc value => mem' *)
  val alloc : 'a t -> Loc.t * 'a t (* get fresh memory cell : Mem.alloc mem => (loc, mem') *)
end

(* Environment Signature *)
module type ENV =
sig
  type ('a, 'b) t
  exception Not_bound
  val empty : ('a, 'b) t (* get empty environment *)
  val lookup : ('a, 'b) t -> 'a -> 'b (* lookup environment : Env.lookup env key => content *)
  val bind : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t  (* id binding : Env.bind env key content => env'*)
end

(* Memory Implementation *)
module Mem : MEM =
struct
  exception Not_allocated
  exception Not_initialized
  type 'a content = V of 'a | U
  type 'a t = M of Loc.t * 'a content list
  let empty = M (Loc.base,[])

  let rec replace_nth = fun l n c ->
    match l with
    | h::t -> if n = 1 then c :: t else h :: (replace_nth t (n - 1) c)
    | [] -> raise Not_allocated

  let load (M (boundary,storage)) loc =
    match (List.nth storage ((Loc.diff boundary loc) - 1)) with
    | V v -> v
    | U -> raise Not_initialized

  let store (M (boundary,storage)) loc content =
    M (boundary, replace_nth storage (Loc.diff boundary loc) (V content))

  let alloc (M (boundary,storage)) =
    (boundary, M (Loc.increase boundary 1, U :: storage))
end

(* Environment Implementation *)
module Env : ENV=
struct
  exception Not_bound
  type ('a, 'b) t = E of ('a -> 'b)
  let empty = E (fun x -> raise Not_bound)
  let lookup (E (env)) id = env id
  let bind (E (env)) id loc = E (fun x -> if x = id then loc else env x)
end

(*
 * K- Interpreter
 *)
module type KMINUS =
sig
  exception Error of string
  type id = string
  type exp =
  | NUM of int | TRUE | FALSE | UNIT
  | VAR of id
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | EQUAL of exp * exp
  | LESS of exp * exp
  | NOT of exp
  | SEQ of exp * exp            (* sequence *)
  | IF of exp * exp * exp       (* if-then-else *)
  | WHILE of exp * exp          (* while loop *)
  | LETV of id * exp * exp      (* variable binding *)
  | LETF of id * id list * exp * exp (* procedure binding *)
  | CALLV of id * exp list      (* call by value *)
  | CALLR of id * id list       (* call by referenece *)
  | RECORD of (id * exp) list   (* record construction *)
  | FIELD of exp * id           (* access record field *)
  | ASSIGN of id * exp          (* assgin to variable *)
  | ASSIGNF of exp * id * exp   (* assign to record field *)
  | READ of id
  | WRITE of exp

  type program = exp
  type memory
  type env
  type value =
  | Num of int
  | Bool of bool
  | Unit
  | Record of (id -> Loc.t)
  val emptyMemory : memory
  val emptyEnv : env
  val run : memory * env * program -> value
end

module K : KMINUS =
struct
  exception Error of string

  type id = string
  type exp =
  | NUM of int | TRUE | FALSE | UNIT
  | VAR of id
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | EQUAL of exp * exp
  | LESS of exp * exp
  | NOT of exp
  | SEQ of exp * exp            (* sequence *)
  | IF of exp * exp * exp       (* if-then-else *)
  | WHILE of exp * exp          (* while loop *)
  | LETV of id * exp * exp      (* variable binding *)
  | LETF of id * id list * exp * exp (* procedure binding *)
  | CALLV of id * exp list      (* call by value *)
  | CALLR of id * id list       (* call by referenece *)
  | RECORD of (id * exp) list   (* record construction *)
  | FIELD of exp * id           (* access record field *)
  | ASSIGN of id * exp          (* assgin to variable *)
  | ASSIGNF of exp * id * exp   (* assign to record field *)
  | READ of id
  | WRITE of exp

  type program = exp

  type value =
  | Num of int
  | Bool of bool
  | Unit
  | Record of (id -> Loc.t)

  type memory = value Mem.t
  type env = (id, env_entry) Env.t
  and  env_entry = Addr of Loc.t | Proc of id list * exp * env

  let emptyMemory = Mem.empty
  let emptyEnv = Env.empty

  let value_int v =
    match v with
    | Num n -> n
    | Bool _ -> raise (Error "TypeError : Num is expected, but Bool")
    | Unit -> raise (Error "TypeError : Num is expected, but Unit")
    | Record _ -> raise (Error "TypeError : Num is expected, but Record")

  let value_bool v =
    match v with
    | Bool b -> b
    | Num _ -> raise (Error "TypeError : Bool is expected, but Num")
    | Unit -> raise (Error "TypeError : Bool is expected, but Unit")
    | Record _ -> raise (Error "TypeError : Bool is expecte, but Record")

  let value_unit v =
      match v with
      | Unit -> ()
      | Num _ -> raise (Error "TypeError : Unit is expected, but Num")
      | Bool _ -> raise (Error "TypeError : Unit is expected, but Bool")
      | Record _ -> raise (Error "TypeError : Unit is expected, but Record")

  let value_record v =
      match v with
      | Record r -> r
      | Num _ -> raise (Error "TypeError : Record is expected, but Num")
      | Bool _ -> raise (Error "TypeError : Record is expected, but Bool")
      | Unit -> raise (Error "TypeError : Record is expected, but Unit")

  let lookup_env_loc e x =
    try
      (match Env.lookup e x with
      | Addr l -> l
      | Proc _ -> raise (Error "TypeError : not addr"))
    with Env.Not_bound -> raise (Error "Unbound")

  let lookup_env_proc e f =
    try
      (match Env.lookup e f with
      | Addr _ -> raise (Error "TypeError : not proc")
      | Proc (id, exp, env) -> (id, exp, env))
    with Env.Not_bound -> raise (Error "Unbound")

  let idEqual id1 id2 =
      if (String.compare id1 id2) == 0 then true
      else false

  let rec eval mem env e =
    match e with
    | NUM i ->
      (Num i, mem)
    | TRUE ->
      (Bool true, mem)
    | FALSE ->
      (Bool false, mem)
    | UNIT ->
      (Unit, mem)
    | VAR (id) ->
      let l = lookup_env_loc env id in
      let v = Mem.load mem l in
      (v, mem)
    | ADD (e1, e2) ->
      let (v1, mem') = eval mem env e1 in
      let (v2, mem'') = eval mem' env e2 in
      let i1 = value_int v1 in
      let i2 = value_int v2 in
      (Num(i1 + i2), mem'')
    | SUB (e1, e2) ->
      let (v1, mem') = eval mem env e1 in
      let (v2, mem'') = eval mem' env e2 in
      let i1 = value_int v1 in
      let i2 = value_int v2 in
      (Num(i1 - i2), mem'')
    | MUL (e1, e2) ->
      let (v1, mem') = eval mem env e1 in
      let (v2, mem'') = eval mem' env e2 in
      let i1 = value_int v1 in
      let i2 = value_int v2 in
      (Num(i1 * i2), mem'')
    | DIV (e1, e2) ->
      let (v1, mem') = eval mem env e1 in
      let (v2, mem'') = eval mem' env e2 in
      let i1 = value_int v1 in
      let i2 = value_int v2 in
      (Num(i1 / i2), mem'')
    | EQUAL (e1, e2) ->
      let (v1, mem') = eval mem env e1 in
      let (v2, mem'') = eval mem' env e2 in
      (match (v1, v2) with
       | (Num(i1), Num(i2)) -> (Bool (i1 == i2), mem'')
       | (Bool(b1), Bool(b2)) -> (Bool (b1 == b2), mem'')
       | (Unit, Unit) -> (Bool true, mem'')
       | _ -> (Bool false, mem''))
    | LESS (e1, e2) ->
      let (v1, mem') = eval mem env e1 in
      let (v2, mem'') = eval mem' env e2 in
      let i1 = value_int v1 in
      let i2 = value_int v2 in
      (Bool(i1 < i2), mem'')
    | NOT (e) ->
      let (v, mem') = eval mem env e in
      let b = value_bool v in
      (Bool(not b), mem')
    | SEQ (e1, e2) ->
      let (_, mem') = eval mem env e1 in
      eval mem' env e2
    | IF (ec, e1, e2) ->
      let (vc, mem') = eval mem env ec in
      if value_bool vc == true then
         eval mem' env e1
      else
         eval mem' env e2
    | WHILE (ec, e) ->
      let (vc, mem') = eval mem env ec in
      if value_bool vc == true then
         let (_, mem'') = eval mem' env e in
         eval mem'' env (WHILE (ec, e))
      else
         (Unit, mem')
    | LETV (x, e1, e2) ->  (* Predefined *)
      let (v, mem') = eval mem env e1 in
      let (l, mem'') = Mem.alloc mem' in
      eval (Mem.store mem'' l v) (Env.bind env x (Addr l)) e2
    | LETF (x, params, fe, e) ->
      let p = Proc(params, fe, env) in
      eval mem (Env.bind env x p) e
    | CALLV (x, vlist) -> (* Value List *)
      let (plist, exp_func, env_func) = lookup_env_proc env x in
      let rec replace = fun mem_func env_func param_list value_list ->
          match (param_list, value_list) with
          | ([], []) -> (mem_func, env_func)
          | ([], _) -> raise (Error "InvalidArg")
          | (_, []) -> raise (Error "InvalidArg")
          | (pl, vl) ->
                  let (loc, mem_func1) = Mem.alloc mem_func in
                  let (v, mem_func2) = eval mem_func1 env (List.hd(vl)) in
                  let mem_func3 = Mem.store mem_func2 loc v in
                  let env_func1 = Env.bind env_func (List.hd(pl)) (Addr loc) in
                  replace mem_func3 env_func1 (List.tl(param_list)) (List.tl(value_list))
      in

      let (mem_func, env_func') = replace mem env_func plist vlist in
      (* for recursive call *)
      let env_func'' = Env.bind env_func' x (Proc (plist, exp_func, env_func)) in
      eval mem_func env_func'' exp_func

    | CALLR (x, rlist) -> (* Reference List *)
      let (plist, exp_func, env_func) = lookup_env_proc env x in
      let rec replace = fun mem_func env_func param_list ref_list ->
          match (param_list, ref_list) with
          | ([], []) -> (mem_func, env_func)
          | ([], _) -> raise (Error "InvalidArg")
          | (_, []) -> raise (Error "InvalidArg")
          | (pl, rl) ->
                  let loc = lookup_env_loc env (List.hd(rl)) in
                  let env_func1 = Env.bind env_func (List.hd(pl)) (Addr loc) in
                  replace mem_func env_func1 (List.tl(param_list)) (List.tl(ref_list))
      in

      let (mem_func, env_func') = replace mem env_func plist rlist in
      (* for recursive call *)
      let env_func'' = Env.bind env_func' x (Proc (plist, exp_func, env_func)) in
      eval mem_func env_func'' exp_func

    | RECORD mlist -> (* Member List - id * exp *)
      let rec make_record_func: (id * Loc.t) list -> (id -> Loc.t) =
          fun id_loc_list ->
              fun i ->
                  let rec fi = fun i l ->
                    if l == [] then raise (Error "Unbound")
                    else if (idEqual (fst(List.hd(l))) i) then snd(List.hd(l))
                    else fi i (List.tl(l))
                  in
                  fi i id_loc_list
      in

      let rec assign_record = fun mem env mlist id_loc_list ->
          if mlist == [] then (mem, id_loc_list)
          else
              let (id, exp) = List.hd(mlist) in
              let (v, mem1) = eval mem env exp in
              let (l, mem2) = Mem.alloc mem1 in
              let mem3 = Mem.store mem2 l v in
              assign_record mem3 env (List.tl(mlist)) ((id, l)::id_loc_list)
      in

      if mlist == [] then (Unit, mem)
      else
          let (mem', id_loc_list) = assign_record mem env mlist [] in
          (Record(make_record_func(id_loc_list)), mem')
    | FIELD (e, id) ->
      let (v, mem') = eval mem env e in
      ((Mem.load mem' ((value_record v) id)), mem')
    | ASSIGN (x, e) ->  (* Predefined *)
      let (v, mem') = eval mem env e in
      let l = lookup_env_loc env x in
      (v, Mem.store mem' l v)
    | ASSIGNF (e, id, rfe) -> (* Record Field EXP *)
      let (v, mem1) = eval mem env e in
      let (rfv, mem2) = eval mem1 env rfe in
      let mem3 = Mem.store mem2 ((value_record v) id) rfv in
      (rfv, mem3)
    | READ x ->   (* Predefined *)
      let v = Num (read_int()) in
      let l = lookup_env_loc env x in
      (v, Mem.store mem l v)
    | WRITE e ->    (* Predefined *)
      let (v, mem') = eval mem env e in
      let n = value_int v in
      let _ = print_endline (string_of_int n) in
      (v, mem')
    (*| _ -> failwith "Unimplemented"*)

  let run (mem, env, pgm) =
    let (v, _ ) = eval mem env pgm in
    v
end

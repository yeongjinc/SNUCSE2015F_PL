
exception IMPOSSIBLE
type treasure = StarBox | NameBox of string
type key = Bar | Node of key * key
type map = End of treasure
            | Branch of map * map
            | Guide of string * map

(* http://ropas.snu.ac.kr/~dreameye/PL/slide/PL12.pdf 참고 *)
type ty = TAlpha of int  (* alpha, beta *)
         | TBar
         | TPair of ty * ty

type constraints = U of map * ty * ty
                   | K of treasure * ty
                   (* 1. Key 를 구하기 위함 2. 같은 NameBox에 조건 추가하기 위함
                    * *)

let next_tvar = ref 0
let newt() = next_tvar := !next_tvar + 1; TAlpha !next_tvar

let subst : int -> ty -> (ty -> ty) =
    fun x tau ->
        let rec s : ty -> ty
        = fun t ->
           match t with
           | TAlpha y -> if y = x then tau else t
           | TBar -> t
           | TPair(t1, t2) -> TPair(s t1, s t2)
        in s

let ( @@ ) g f = (fun t -> g (f t))    (* 치환의 합성 *)
(* 어차피 ty -> ty 이기 때문에 ty -> ty -> ty 는
 * 두 번 호출된다기보다 각 경우를 합쳐준다고 생각하면 될 듯 *)
let id = (fun x -> x)

let ( @+ ) g (x, t) = fun y -> if y = x then t else g y
let emptyG = fun x -> raise IMPOSSIBLE

let rec occurs : int -> ty -> bool =
    fun x tau ->
        match tau with
        | TAlpha y -> if y = x then true else false
        | TPair (t1, t2) -> occurs x t1 || occurs x t2
        | TBar -> false

let rec unify : ty -> ty -> (ty -> ty) =
    fun t1 t2 ->
        match (t1, t2) with
        | (TAlpha x, tau) ->
            if TAlpha x = tau then id
            else if not (occurs x tau) then subst x tau
            else raise IMPOSSIBLE
        | (tau, TAlpha x) -> unify (TAlpha x) tau
        | (TPair(a, b), TPair(c, d)) -> unifypair (a, b) (c, d)
        | (tau, tau') ->
            if tau = tau' then id
            else raise IMPOSSIBLE
    and unifypair (t1, t2) (t1', t2') =
        let s = unify t1 t1' in
        let s' = unify (s t2) (s t2') in
            s' @@ s


let rec v : (treasure -> ty) -> map -> ty -> constraints list -> constraints list =
    fun g m tau ->
        let u tau tau' = (fun c -> U (m, tau, tau') :: c) in
        let k tre tau = (fun c -> K(tre, tau) :: c) in
        match m with
        | End(t) ->
            (match t with
            | StarBox -> u tau TBar @@ k t tau
            | NameBox(s) -> (try(u tau (g t) @@ k t tau) with (IMPOSSIBLE) -> u tau
            (newt()) @@ k t tau)
            )
        | Branch(m1, m2) ->
            let alpha = newt() in
            let beta = newt() in
            (* beta is tau (전체 모양이 암시하는 것이 베타) *)
            u tau beta @@ v g m1 (TPair(alpha, beta)) @@ v g m2 alpha
        | Guide(s, m') ->
            let alpha = newt() in
            let beta = newt() in
            u tau (TPair(alpha, beta)) @@ v (g @+ (NameBox(s), alpha)) m' beta

let rec c2s : (ty -> ty) -> constraints list -> (ty -> ty) =
    fun s cl ->
        match cl with
        | [] -> s
        | (U (e, t1, t2) :: c) -> c2s (unify (s t1) (s t2) @@ s) c
        | (K(dummy1, dummy2) :: c) -> c2s s c (* Key 표시는 무시 *)

(*
let getType: map -> ty =
    fun map ->
        let tau = newt() in
        ((c2s id (v emptyG map tau [])) tau)
*)

let getReady: map -> key list =
    fun map ->
       let tau = newt() in
       let clist_beforename : constraints list = v emptyG map tau [] in

       (* NameBox 이름 같을 시 같은 키로 열린다는 조건 *)
       let rec getNameConstraint : treasure -> ty ->  constraints list -> constraints
       list =
           fun tre_ori tau_ori cl ->
               match cl with
               | [] -> []
               | (U(dummy1, dummy2, dummy3) :: c) -> getNameConstraint tre_ori
               tau_ori c
               | (K(tre, tau) :: c) ->
                       if tre = tre_ori then U(map, tau,
                       tau_ori)::(getNameConstraint tre_ori tau_ori c)
                       else getNameConstraint tre_ori tau_ori c
       in
       let rec getAllNameConstraint : constraints list -> constraints list =
           fun cl ->
               match cl with
               | [] -> []
               | (U(dummy1, dummy2, dummy3) :: c) -> getAllNameConstraint c
               | (K(tre, tau) :: c) ->
                       (match tre with
                       | NameBox(s) -> (getNameConstraint tre tau c)@(getAllNameConstraint c)
                       | _ -> getAllNameConstraint c
                       )
       in

       let clist : constraints list = clist_beforename @ (getAllNameConstraint
       clist_beforename) in
       let tlist : ty -> ty = c2s id clist in
       let rec getKeyType : (ty->ty) -> ty -> key =
           fun f t ->
               match t with
               | TAlpha(i) ->
                       if (f t) = TAlpha(i) then Bar
                       else (getKeyType f (f t))
               | TBar -> Bar
               | TPair(t1, t2) -> Node((getKeyType f t1), (getKeyType f t2))
       in
       let rec getAllKeyType : constraints list -> (ty -> ty) -> key list =
           fun cl tl ->
               match cl with
               | [] -> []
               | (U(dummy1, dummy2, dummy3) :: c) -> getAllKeyType c tl
               | (K(tre, tau) :: c) -> (getKeyType tl tau)::(getAllKeyType c tl)
       in
       (* sort? *)
       let rec removeDuplicate : 'a list -> 'a list =
           fun l ->
               match l with
               | hd::tl -> if List.mem hd tl then (removeDuplicate tl) else
                   hd::(removeDuplicate tl)
               | [] -> []
       in
       removeDuplicate (getAllKeyType clist tlist)




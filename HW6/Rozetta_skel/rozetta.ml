(*
 * SNU 4190.310 Programming Languages
 * Homework "Rozetta" Skeleton
 * Jaeseung Choi (jschoi@ropas.snu.ac.kr)
 *)
let first_call = ref true (* Sonata로 변환 시 첫 번째 콜을 제외한 모든 콜은 함수 안쪽의 콜 *)
let trans_v : Sm5.value -> Sonata.value = function
  | Sm5.Z z  -> Sonata.Z z
  | Sm5.B b  -> Sonata.B b
  | Sm5.L _ -> raise (Sonata.Error "Invalid input program : pushing location")
  | Sm5.Unit -> Sonata.Unit
  | Sm5.R _ -> raise (Sonata.Error "Invalid input program : pushing record")

(* TODO : complete this function *)
let rec trans_obj : Sm5.obj -> Sonata.obj = function
  | Sm5.Val v -> Sonata.Val (trans_v v)
  | Sm5.Id id -> Sonata.Id id
  | Sm5.Fn (arg, command) ->
          let c = "#yjcon" in
          Sonata.Fn (arg, [Sonata.BIND c] @ (trans' command) @
          [Sonata.PUSH (Sonata.Id c); Sonata.PUSH (Sonata.Val (Sonata.Unit)); Sonata.MALLOC;
          Sonata.UNBIND; Sonata.POP; Sonata.CALL])

(* TODO : complete this function *)
and trans' : Sm5.command -> Sonata.command = function
  | Sm5.PUSH obj :: cmds -> Sonata.PUSH (trans_obj obj) :: (trans' cmds)
  | Sm5.POP :: cmds -> Sonata.POP :: (trans' cmds)
  | Sm5.STORE :: cmds -> Sonata.STORE :: (trans' cmds)
  | Sm5.LOAD :: cmds -> Sonata.LOAD :: (trans' cmds)
  | Sm5.JTR (c1, c2) :: cmds ->  Sonata.JTR (trans' c1, trans' c2) :: (trans' cmds)
  | Sm5.MALLOC :: cmds -> Sonata.MALLOC :: (trans' cmds)
  | Sm5.BOX z :: cmds -> Sonata.BOX z :: (trans' cmds)
  | Sm5.UNBOX id :: cmds -> Sonata.UNBOX id :: (trans' cmds)
  | Sm5.BIND id :: cmds -> Sonata.BIND id :: (trans' cmds)
  | Sm5.UNBIND :: cmds -> Sonata.UNBIND :: (trans' cmds)
  | Sm5.GET ::cmds -> Sonata.GET :: (trans' cmds)
  | Sm5.PUT ::cmds -> Sonata.PUT :: (trans' cmds)
  | Sm5.CALL :: cmds ->
        let l = "#yjloc" in (* 디버깅을 위해 특정 문자열 붙임 *)
        let v = "#yjvar" in
        let f = "#yjfun" in
        let x = "#yjx" in
        let c = "#yjcon" in
        let ret =
            let first = !first_call in
            first_call := false;
            if first then []
            else
                 [Sonata.PUSH (Sonata.Id c); Sonata.PUSH (Sonata.Val (Sonata.Unit));
                  Sonata.MALLOC; Sonata.CALL]
        in
        [Sonata.PUSH (Sonata.Fn (x, (trans' cmds) @ ret))]
        @ [Sonata.BIND c]
        @ [Sonata.MALLOC; Sonata.BIND l; Sonata.PUSH(Sonata.Id l); Sonata.STORE]
        @ [Sonata.MALLOC; Sonata.BIND v; Sonata.PUSH(Sonata.Id v); Sonata.STORE]
        @ [Sonata.BIND f] (* POP은 재귀때문에 두 번 넣기 때문이다 -> 바로 bind하는 구조로 변경하면서 뺌 *)
        @ [Sonata.PUSH (Sonata.Id c)]
        @ [Sonata.PUSH (Sonata.Id f);
            Sonata.PUSH (Sonata.Id v); Sonata.LOAD;
            Sonata.PUSH (Sonata.Id l); Sonata.LOAD]
        @ [Sonata.UNBIND; Sonata.POP; Sonata.UNBIND; Sonata.POP; Sonata.UNBIND; Sonata.POP; Sonata.UNBIND; Sonata.POP]
        @ [Sonata.CALL]
  | Sm5.ADD :: cmds -> Sonata.ADD :: (trans' cmds)
  | Sm5.SUB :: cmds -> Sonata.SUB :: (trans' cmds)
  | Sm5.MUL :: cmds -> Sonata.MUL :: (trans' cmds)
  | Sm5.DIV :: cmds -> Sonata.DIV :: (trans' cmds)
  | Sm5.EQ :: cmds -> Sonata.EQ :: (trans' cmds)
  | Sm5.LESS :: cmds -> Sonata.LESS :: (trans' cmds)
  | Sm5.NOT :: cmds -> Sonata.NOT :: (trans' cmds)
  | [] -> []

(* TODO : complete this function *)
let trans : Sm5.command -> Sonata.command = fun command ->
  trans' command

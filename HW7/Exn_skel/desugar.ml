(*
 * SNU 4190.310 Programming Languages
 * Homework "Exceptions are sugar" Skeleton
 * Jaeseung Choi (jschoi@ropas.snu.ac.kr)
 *)

open Xexp

(* TODO : alpha_conv 필요한지 고민해 볼 것 *)
let count = ref 0
let new_name () =
    let _ = count := !count + 1 in
    "yj_" ^ (string_of_int !count)


let initialK =
    let ie = new_name() in
    let v = new_name() in
    Fn (ie,
        (If (Equal (Var ie, Num 0),
            Fn (v, Var v),
            Fn (v, Num 201511))
        )
    )


let addHandler k n h =
    let ie = new_name() in
    let v = new_name() in
    Fn (ie,
        If (Equal (Var ie, Num 0),
            App (Var k, Num 0),
            Fn (v,
                If (Equal (Var v, Num n),
                    App (h, Var k),
                    App (App (Var k, Num 1), Num 1)
                )
            )
        )
    )


(* cps = (Int->Int->Result) -> Result *)
let rec cps' xe =
    let k = new_name () in
    match xe with
    | Num n -> Fn (k, App (App (Var k, Num 0), Num n))
    | Var x -> Fn (k, App (App (Var k, Num 0), Var x))
    | Fn (x, e) -> Fn (k, App (App (Var k, Num 0), Fn(x, e)))
    | Raise e ->
            let ie = new_name() in (* isException *)
            let v = new_name() in
            Fn (k,
                App (cps' e,
                    (* Int->Int->Result *)
                    Fn (ie,
                        If (Equal (Var ie, Num 0),
                            (* Int->Result *)
                            Fn (v,
                                App (App (Var k, Num 1), Var v)
                            )
                            ,
                            Fn (v,
                                App (App (Var k, Num 1), Var v)
                            )
                        )
                    )
                )
            )
    | Handle (e1, n, e2) ->
            let k' = addHandler k n (cps' e2) in
            Fn (k,
                App (cps' e1, k')
            )
    | App (e1, e2) ->
            let ie1 = new_name () in
            let ie2 = new_name () in
            let v1 = new_name() in
            let v2 = new_name() in
            Fn (k,
                App (cps' e1,
                    Fn (ie1,
                        If (Equal (Var ie1, Num 0),
                            Fn (v1,
                                App (cps' e2,
                                    Fn (ie2,
                                        If (Equal (Var ie2, Num 0),
                                            Fn (v2,
                                                App (App (Var k, Num 0), App (Var v1, Var v2))
                                            ),
                                            Fn (v2,
                                                App (App (Var k, Num 1), Var v2)
                                            )
                                        )
                                    )
                                )
                            ),
                            Fn (v1,
                                App (App (Var k, Num 1), Var v1)
                            )
                        )
                    )
                )
            )
    | If (e1, e2, e3) ->
            let ie1 = new_name () in
            let ie2 = new_name () in
            let v1 = new_name() in
            let v2 = new_name() in
            Fn (k,
                App (cps' e1,
                    Fn (ie1,
                        If (Equal (Var ie1, Num 0),
                            Fn (v1,
                                If (Var v1,
                                    App (cps' e2,
                                        Fn (ie2,
                                            If (Equal (Var ie2, Num 0),
                                                Fn (v2,
                                                    App (App (Var k, Num 0), Var v2)
                                                ),
                                                Fn (v2,
                                                    App (App (Var k, Num 1), Var v2)
                                                )
                                            )
                                        )
                                    ),
                                    App (cps' e3,
                                        Fn (ie2,
                                            If (Equal (Var ie2, Num 0),
                                                Fn (v2,
                                                    App (App (Var k, Num 0), Var v2)
                                                ),
                                                Fn (v2,
                                                    App (App (Var k, Num 1), Var v2)
                                                )
                                            )
                                        )
                                    )
                                )
                            ),
                            Fn (v1,
                                App (App (Var k, Num 1), Var v1)
                            )
                        )
                    )
                )
            )
    | Equal (e1, e2) ->
            let ie1 = new_name () in
            let ie2 = new_name () in
            let v1 = new_name() in
            let v2 = new_name() in
            Fn (k,
                App (cps' e1,
                    Fn (ie1,
                        If (Equal (Var ie1, Num 0),
                            Fn (v1,
                                App (cps' e2,
                                    Fn (ie2,
                                        If (Equal (Var ie2, Num 0),
                                            Fn (v2,
                                                App (App (Var k, Num 0), Equal (Var v1, Var v2))
                                            ),
                                            Fn (v2,
                                                App (App (Var k, Num 1), Var v2)
                                            )
                                        )
                                    )
                                )
                            ),
                            Fn (v1,
                                App (App (Var k, Num 1), Var v1)
                            )
                        )
                    )
                )
            )



(* TODO : Implement this function *)
let removeExn : xexp -> xexp = fun e ->
    App ((cps' e), initialK)

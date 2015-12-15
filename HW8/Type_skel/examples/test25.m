let val foo = fn x => fn y =>
  if x = y then x else y 
in
  let val i = 1
      val s = "hello world" 
      val b = true 
      val l = malloc 10
  in
    (
      (foo i 2, foo s "bye world"), 
      (foo b false, foo (5,1) (1,2))
    )
  end
end

(* Reseult : ((int, string), (bool, loc (int))) *)

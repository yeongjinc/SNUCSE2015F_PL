
let val f = fn x => (x, (write x)) in
  malloc (malloc 10);
  ((f 1), (f true))
end

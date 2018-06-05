(*
  Reducers (interpreters) for lambda-calculus.
*)

open Utils
open Parser


exception OutOfVariablesError


let possible_variables = List.map (fun x -> char_to_string (char_of_int x)) ((range 97 123) @ (range 65 91))



(*
  ADD FUNCTIONS BELOW
18*)
let rec fv :term ->StringSet.t = function
     | Variable v -> StringSet.add  v StringSet.empty
     | Abstraction(a,t) -> StringSet.remove a (fv(t))
     | Application(t1,t2) -> StringSet.union (fv(t1)) (fv(t2))

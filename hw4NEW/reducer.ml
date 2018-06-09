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

let fresh_var (names_in_use: StringSet.t) =
        let possible_variables_set = StringSet.of_list possible_variables in
        let unused_names = StringSet.diff possible_variables_set names_in_use in
        if StringSet.is_empty unused_names then raise OutOfVariablesError else StringSet.choose unused_names

let rec substitute (x: string) (t1: term) (t2:term)  = match t2 with
        | Variable y -> if x=y then t1 else Variable y
        | Abstraction (y, t2) -> if x=y then Abstraction (y, t2) 
                                 else if not (StringSet.mem y (fv t1)) then Abstraction(y, substitute x t1 t2)
                                 else 
                                         let tmp_set = StringSet.union (fv t1) (fv t2) in
                                         let unioned_set = StringSet.add x tmp_set in
                                         let z = fresh_var unioned_set in
                                         let s1 = substitute y (Variable z) t2 in
                                         let s2 = substitute x t1 s1 in
                                         Abstraction (z,s2)
        | Application (s1, s2) -> Application ((substitute x t1 s1), (substitute x t1 s2))

let rec reduce_strict (t: term) : term option = match t with
        | Variable x -> None 
        | Abstraction(x, s) -> None
        | Application(t1, t2) -> let t1_optional = reduce_strict t1 in (match t1_optional with
                | Some t1_tmp -> Some (Application(t1_tmp, t2))
                | None -> let t2_optional = reduce_strict t2 in (match t2_optional with
                        | Some t2_tmp -> Some (Application(t1, t2_tmp))
                        | None -> match t with
                                | Application(Abstraction(y, t12), t2) -> Some(substitute y t2 t12)
                                | _ -> None
                )
        )


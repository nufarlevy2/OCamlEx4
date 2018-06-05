(*
  Reducers (interpreters) for lambda-calculus.
*)

open Utils
open Parser


exception OutOfVariablesError


let possible_variables = List.map (fun x -> char_to_string (char_of_int x)) ((range 97 123) @ (range 65 91))



(*
  ADD FUNCTIONS BELOW
*)

let rec fv = function
    | Variable id -> StringSet.singleton id
    | Abstraction (id, term) -> StringSet.remove id (fv term) 
    | Application (t1, t2) -> StringSet.union (fv t1) (fv t2)

let possible_variables_set = StringSet.of_list possible_variables

let fresh_var used_names = 
    let availble_names = StringSet.diff possible_variables_set used_names
    in 
        if StringSet.is_empty availble_names then raise OutOfVariablesError
        else StringSet.choose availble_names


let rec substitute x s t = match t with
    | Variable y -> if x=y then s else Variable y
    
    | Abstraction (y, t) -> if x=y then Abstraction (y, t)
                            else if not (StringSet.mem y (fv s)) then
                                Abstraction(y, substitute x s t)
                            else 
                                let set1 = StringSet.union (fv s) (fv t) in
                                let set2 = StringSet.add x set1 in
                                let z = fresh_var set2 in
                                let t1 = substitute y (Variable z) t in
                                let t2 = substitute x s t1
                                in Abstraction (z, t2)

    | Application (t1, t2) -> Application ((substitute x s t1), (substitute x s t2))

let rec reduce_strict (term : term) : term option = match term with
    | Variable id -> None
    | Abstraction(id, t) -> None

    (* | Application(Abstraction(x, t12), t2) -> Some(substitute x t2 t12)
 *)
    | Application(t1, t2) -> let t1'_opt = reduce_strict t1 in ( match t1'_opt with
                                | Some t1' -> Some (Application (t1', t2))
                                | None -> let t2'_opt = reduce_strict t2 in ( match t2'_opt with
                                        | Some t2' -> Some (Application(t1, t2'))
                                        | None -> match term with
                                            | Application(Abstraction(x, t12), t2) -> Some(substitute x t2 t12)
                                            | _ -> None
                                        )
                                )
    


let rec reduce_lazy : term -> term option = function
    | Variable id -> None
    | Abstraction(id, t) -> None
    | Application(t1, t2) -> let t1'_opt = reduce_lazy t1 in(
                                match t1'_opt with 
                                | Some t1' -> Some (Application(t1', t2))
                                | None -> match t1 with
                                    | Abstraction(x, t12) -> Some (substitute x t2 t12)
                                    | _ -> None
                                )


let rec reduce_normal : term -> term option = function
    | Variable id -> None
    | Application(Abstraction(x, t12), t2) -> Some(substitute x t2 t12)

    | Abstraction(id, t) -> let t'_opt = reduce_normal t in (
                                match t'_opt with
                                | Some t' -> Some (Abstraction (id, t'))
                                | None -> None
                                )

    | Application(t1, t2) -> let t1'_opt = reduce_normal t1 in match t1'_opt with
                                | Some t1' -> Some (Application (t1', t2))
                                | None -> let t2'_opt = reduce_normal t2 in match t2'_opt with
                                        | Some t2' -> Some (Application(t1, t2'))
                                        | None -> None


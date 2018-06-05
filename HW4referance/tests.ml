(*
  Tests for the lambda calculus parser and reducers.

  EXTEND THIS FILE TO TEST YOUR SOLUTION THOROUGHLY!
*)

open Utils
open Parser
open Reducer

let rec evaluate ~verbose reduce t =
  if verbose then print_string (format_term t) else ();
  match reduce t with
  | None ->
    if verbose then print_string " =/=>\n\n" else ();
    t
  | Some t' ->
    if verbose then print_string " ==>\n\n" else ();
    evaluate ~verbose reduce t'


let test_and_1 = "
let tru = (\\t.(\\f.t)) in
let fls = (\\t.(\\f.f)) in
let and = (\\b.(\\c. ((b c) fls))) in
((and tru) tru)
"

let test_and_2 = "
let tru = (\\t.(\\f.t)) in
let fls = (\\t.(\\f.f)) in
let and = (\\b.(\\c. ((b c) fls))) in
((and fls) ((and tru) tru))
"

let env = "
let tru = (\\t. (\\f. t)) in
let fls = (\\t. (\\f. f)) in
let test = (\\l. (\\m. (\\n. ((l m) n)))) in
let and = (\\b. (\\c.  ((b c) fls))) in

let pair = (\\f. (\\s. (\\b.  ((b f) s)))) in
let fst = (\\p. (p tru)) in
let snd = (\\p. (p fls)) in

let c0 = (\\s. (\\z. z)) in
let c1 = (\\s. (\\z. (s z))) in
let c2 = (\\s. (\\z. (s (s z)))) in
let c3 = (\\s. (\\z. (s (s (s z))))) in
let c4 = (\\s. (\\z. (s (s (s (s z)))))) in
let c5 = (\\s. (\\z. (s (s (s (s (s z))))))) in
let c6 = (\\s. (\\z. (s (s (s (s (s (s z)))))))) in
let c7 = (\\s. (\\z. (s (s (s (s (s (s (s z))))))))) in
let c8 = (\\s. (\\z. (s (s (s (s (s (s (s (s z)))))))))) in
let c9 = (\\s. (\\z. (s (s (s (s (s (s (s (s (s z))))))))))) in
let c10 = (\\s. (\\z. (s (s (s (s (s (s (s (s (s (s z)))))))))))) in

let scc = (\\n. (\\s. (\\z. (s ((n s) z))))) in
let plus = (\\m. (\\n. (\\s. (\\z. ((m s) ((n s) z)))))) in
let times = (\\m. (\\n. (\\s. (m (n s))))) in
let power = (\\m. (\\n. (n m))) in
let iszero = (\\m. ((m (\\x. fls)) tru)) in
let prd = (let zz = ((pair c0) c0) in
           let ss = (\\p. ((pair (snd p)) ((plus c1) (snd p)))) in
           (\\m. (fst ((m ss) zz)))) in
let leq = (\\m. (\\n. (iszero ((n prd) m)))) in
let equal = (\\m. (\\n. ((and ((leq m) n)) ((leq n) m)))) in

let Y = (\\f. ((\\x. (f (x x))) (\\x. (f (x x))))) in
let Z = (\\f. ((\\x. (f (\\y. ((x x) y)))) (\\x. (f (\\y. ((x x) y)))))) in
"

let test_fact_l = env ^ "
let fact_l = (Y (\\f. (\\n. (((test (iszero n)) c1) (((times n) (f (prd n)))))))) in
((equal (fact_l c2)) c2)
"

let test_fact_s = env ^ "
let fact_s = (Z (\\f. (\\n. ((((test (iszero n)) (\\x. c1)) (\\x. (((times n) (f (prd n)))))) (\\x.x))))) in
((equal (fact_s c2)) c2)
"


let test ~verbose ~sem ~reduce s =
  printf "\nEvaluating:\n%s\nin %s semantics:\n\n" s sem;
  let result = (evaluate ~verbose reduce (parse s)) in
  printf "Result is: %s\n\n" (format_term result)

let test_lazy = test ~sem:"lazy" ~reduce:reduce_lazy
let test_strict = test ~sem:"strict" ~reduce:reduce_strict
let test_normal = test ~sem:"normal-order" ~reduce:reduce_normal
let test_all ~verbose s =
  test_lazy ~verbose s;
  test_strict ~verbose s;
  test_normal ~verbose s

(* MY ADDITIONS *)
let test_bonus_a = "\\x. \\y. \\z. x"
let test_bonus_a3 = "\\x. \\y. \\z. x y z"
let test_bonus_a2 = "x y z"
let test_bonus_a4 = "(x y z)"
let test_bonus_a5 = "(x y z) w"
let test_bonus_a6 = "(x y z) (w a)"

let test_bonus7 = "\\x.x (a b)"

(* let str_to_tok = s |> string_to_list |> tokenize *)
let do_test x =  x |> parse |> format_term |> print_endline
let do_test_conv x =  x |> parse_conv |> format_term |> print_endline

let print_term term = term |> format_term |> print_endline
let print_string_set_1 = StringSet.iter (fun x -> print_string (x^" ") ) 

let () =
(*   test_all ~verbose:true test_and_1;
  test_all ~verbose:true test_and_2;

  test_lazy ~verbose:false test_fact_l;
  test_strict ~verbose:false test_fact_s;
  test_normal ~verbose:false test_fact_l;
  test_normal ~verbose:false test_fact_s
 *)
 print_string "test parse_term\n"; 
 do_test "x";
 do_test "(\\x. y)";
 do_test "(t1 t2)";
 do_test "(t)";
 do_test "let id =t1 in t2";

 print_string "\n";
 print_string "test fv:\n";
 (Variable "x") |> fv |> print_string_set;
 Abstraction("x", (Variable "x")) |> fv |> print_string_set;
 Abstraction("x", (Variable "y")) |> fv |> print_string_set;
 Application((Variable "x"), (Variable "y")) |> fv |> print_string_set_1;

 print_string "\n";
 print_string "test fresh_var:\n";
 print_endline (fresh_var StringSet.empty);
 print_endline (fresh_var StringSet.empty);
 print_endline (fresh_var (StringSet.singleton "A"));
 print_endline (fresh_var (string_set_from_list ["A"; "B"; "C"]));
 
 print_string "\n";
 print_string "test substitute:\n";
 (substitute "x" (Variable "s") (Variable "x")) |> print_term;
 (substitute "x" (Variable "s") (Variable "y")) |> print_term;
 (substitute "x" (Variable "s") (Application( (Variable "x"), (Variable "y")))) |> print_term;
 (substitute "x" (Variable "s") (Abstraction("x", (Variable "x")))) |> print_term;
 (substitute "x" (Variable "s") (Abstraction("y", (Variable "x")))) |> print_term;
 (substitute "x" (Application((Variable "s"), (Variable "y") )) (Abstraction("y", (Variable "x")))) |> print_term;
 (substitute "x" (Application((Variable "s"), (Variable "y") )) (Abstraction("y", Application((Variable "x"), (Variable "y"))))) |> print_term;


 print_string "\n\n";
 print_string "test A\n";
 do_test_conv test_bonus_a;
 do_test_conv test_bonus_a2;
 do_test_conv test_bonus_a4;
 do_test_conv test_bonus_a5;
 do_test_conv test_bonus_a6;
 do_test_conv test_bonus_a3;
 do_test_conv test_bonus7;

 print_string "\n";
 print_string "test B:\n";
 do_test_conv "(\\x.x) (\\y.y)";
 do_test_conv "(t (\\y.y)) s";
 do_test_conv "a (b c)";
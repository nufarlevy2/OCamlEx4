(*
  Parser for lambda-calculus.
*)

open Utils
open Lexer


(* AST for lambda expressions - DO NOT MODIFY THIS TYPE *)
type term = | Variable of string
	    | Abstraction of string * term
	    | Application of term * term

(*
  Concrete Syntax:
  t ::= id | (\id.t) | (t1 t2) | (t) | let id=t1 in t2

  Abstract Syntax:
  term ::= id | \id.term | term1 term2
*)

exception SyntaxError of string

(*
  ADD FUNCTIONS BELOW
*)

let consume_rparen = function
	| RParen :: ts -> ts
	| _ -> raise (SyntaxError "Expected right parentesis.\n")


let rec parse_term = function
  (* variable *)
  | (Literal s) :: ts -> ((Variable s), ts)

  (* lambda *)
  | LParen :: LambdaTok :: ts -> let lambda, ts' = parse_lambda ts in (lambda, consume_rparen(ts'))
  
  (* let expression *)
  | LetTok :: ts -> parse_let ts

  
  (* application or parenthesis *)
  | LParen :: ts -> parse_application_or_paren ts

  | [] -> raise (SyntaxError "Expected term.\n")
  | _ -> raise (SyntaxError "bad token.\n")


and parse_lambda = function
	| Literal s :: DotTok :: ts -> let t, ts' = parse_term ts in (Abstraction (s, t), ts')
	| _ -> raise (SyntaxError "Illegal input for lambda.\n")

(* let id=t1 in t2  -> (\id. t2) t1 *)
and parse_let = function
	| Literal id :: EqTok :: ts -> let t1, ts' = parse_term ts in (
		match ts' with 
		| InTok :: ts'' -> let t2, ts''' = parse_term ts'' in (Application(Abstraction(id, t2), t1), ts''')
		| _ -> raise (SyntaxError "let expression: exptected in.\n")
	)
	| _ -> raise (SyntaxError "let expression: exptected 'id='.\n")	

(* (t1 t2) | (t) *)
and parse_application_or_paren ts =
	let t1, ts' = parse_term ts in match ts' with
		| RParen :: ts'' -> (t1, ts'')
		| _ -> let t2, ts'' = parse_term ts' in (Application(t1, t2), consume_rparen(ts''))
	

let parse s =
	let (r, ts) =
		s |> string_to_list |> tokenize |> parse_term
	in
	match ts with
		| [] -> r
		| _ -> raise (SyntaxError "Unexpected input.\n")

let rec format_term : term -> string = function
	| Variable id -> id
	| Abstraction (id, term) -> "(\\" ^ id ^ ". " ^ format_term term ^ ")"
	| Application (t1, t2) -> "(" ^ format_term t1 ^ " " ^ format_term t2 ^ ")"



(*
  Concrete Syntax:
  t ::= id | (\id.t) | (t1 t2) | (t) | let id=t1 in t2

  Abstract Syntax:
  term ::= id | \id.term | term1 term2
*)

(* BONOUS *)
(* bonus A *)
let join_apps apps x =
	match apps with
	| None -> Some x
	| Some apps' -> Some (Application(apps', x))

let get_term = function
	| None -> raise (SyntaxError "expected term.\n")
	| Some term -> term

let rec parse_term_conv ts =
  collect_terms ts None

and start_paren ts = 
  let t, ts' = parse_term_conv ts in (t, consume_rparen ts')

and collect_terms (ts : token list) (terms : term option) : term * token list = match ts with
  | LParen::ts' -> let term, ts'' = start_paren ts' in collect_terms ts'' (join_apps terms term)
  
  | InTok::ts' -> (get_term terms), InTok::ts'
  | RParen::ts' -> (get_term terms), RParen::ts'
  | [] -> (get_term terms), []

  | (Literal s) :: ts -> let terms' = join_apps terms (Variable s) in (collect_terms ts terms')

  (* lambda *)
  | LambdaTok :: ts -> parse_lambda_conv ts

  (* let expression *)
  | LetTok :: ts -> parse_let_conv ts

  | _ -> raise (SyntaxError "bad token.\n")

and parse_lambda_conv = function
	| Literal s :: DotTok :: ts -> let t, ts' = parse_term_conv ts in (Abstraction (s, t), ts')
	| _ -> raise (SyntaxError "Illegal input for lambda.\n")

and parse_let_conv = function
	| Literal id :: EqTok :: ts -> let t1, ts' = parse_term_conv ts in (
		match ts' with 
		| InTok :: ts'' -> let t2, ts''' = parse_term_conv ts'' in (Application(Abstraction(id, t2), t1), ts''')
		| _ -> raise (SyntaxError "let expression: exptected in.\n")
	)
	| _ -> raise (SyntaxError "let expression: exptected 'id='.\n")


(* bonus B *)
let parens s = "(" ^ s ^ ")"

let rec format_term_conv_impl (t: term) is_left_in_app = match t with
	| Variable id -> id
	| Abstraction (id, term) -> "\\" ^ id ^ ". " ^ format_term_conv_impl term false

	| Application(t, Application(t1, t2)) -> let s1 = parens_if_abstraction t true in
											 let s2 = format_term_conv_impl (Application(t1, t2)) false
											 in s1 ^ " " ^ parens(s2)

	| Application(t1, t2) -> let s1 = parens_if_abstraction t1 true in
							 let s2 = if is_left_in_app then parens_if_abstraction t2 false
							 		  else format_term_conv_impl t2 false
							 in s1 ^ " " ^ s2

and parens_if_abstraction t is_left_in_app = match t with
	| Abstraction(id, t') -> let term_str = format_term_conv_impl (Abstraction(id, t')) is_left_in_app in
							 parens(term_str)
	| _ -> format_term_conv_impl t is_left_in_app							

let format_term_conv term = 
	format_term_conv_impl term false

let parse_conv s =
  let (r, ts) =
    s |> string_to_list |> Lexer.tokenize |> parse_term_conv
  in
  match ts with
    | [] -> r
    | _ -> raise (SyntaxError "Unexpected input.\n")

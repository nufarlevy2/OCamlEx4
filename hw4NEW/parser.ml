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
let rec is_right_paren = function (*27*)
    | RParen :: ts -> ts
    | _ -> raise (SyntaxError "no right paren.\n")

let rec parse_term = function
    | (Literal l) :: ts -> ((Variable l), ts) (* ID *)
    | LParen :: LambdaTok :: ts -> let lambda, ts' = parse_lambda ts in (lambda,is_right_paren(ts')) (* LAMBDA *)
    | LParen :: ts -> parse_app_or_single ts (* (t) or (t1 t2) *)
    | LParen :: LetTok :: ts -> parse_let ts (* LET *)
    | [] -> raise (SyntaxError "term is not complete.\n")
    | _ -> raise (SyntaxError "invalid term.\n")
    
and parse_lambda = function
    | Literal x :: DotTok :: ts -> let term,ts' = parse_term(ts) in (Abstraction(x,term),ts')
    | _ -> raise (SyntaxError "invalid lambda term.\n")
    
and parse_app_or_single ts = 
    let term,ts' = parse_term ts in match ts' with
    | RParen ::ts'' -> (term,ts'') (* (t) *)
    | _ -> let t2,ts'' = parse_term(ts') in (Application(term,t2),is_right_paren(ts'')) (* t1 t2 *)

and parse_let = function
    | Literal x :: EqTok :: ts -> let t1,ts' = parse_term(ts) in (match ts' with
        | InTok :: ts'' -> let t2,ts'''=parse_term(ts'') in (Application(Abstraction(x,t2),t1),ts''')
        | _ -> raise (SyntaxError "invalid let.\n")
        )
    | _ -> raise (SyntaxError "invalid let.\n")

let parse s = 
     let (r,ts)  = 
        s |> string_to_list |> tokenize |> parse_term 
    in  match ts with
        | [] -> r
        | _ -> raise (SyntaxError "Unexpected input.\n")

let rec format_term  = function
      | Variable v -> v
      | Abstraction(a,t) -> "\\" ^ a ^ ".(" ^ format_term t ^ ")"
      | Application(t1,t2) -> "(" ^ format_term t1 ^ " " ^ format_term t2 ^ ")"
	 
	 
let add_parenthesis t = 
	"(" ^ t ^ ")" 
	  
let rec format_term_conv term = match term with
	  | Variable v -> v
      | Abstraction(a,t) -> "\\" ^ a ^ "." ^ (format_term_conv t)
      | Application(Abstraction(a,t1),Application(t2,t3)) -> add_parenthesis("\\" ^ a ^ "." ^ (format_term_conv t1)) ^ (add_parenthesis(format_term_conv (Application(t2,t3))))
      | Application(t1,Application(t2,t3)) -> format_term_conv t1  ^ " " ^ add_parenthesis (format_term_conv (Application(t2,t3)))
	  | Application(Abstraction(a,t1),t2) -> add_parenthesis("\\" ^ a ^ "." ^ format_term_conv t1) ^ (format_term_conv t2)
	  | Application(t1,Abstraction(a,t2)) -> (format_term_conv t1) ^ (add_parenthesis ("\\" ^ a ^ "." ^ format_term_conv t2))
	  | Application(t1,t2) -> (format_term_conv t1) ^ " " ^ (format_term_conv t2)
	 

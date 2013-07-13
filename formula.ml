(* This file is distributed under the terms of
   the GNU Lesser General Public License Version 2.1 *)
(* Hyungchul Park, Sungwoo Park *)
(* Programming Language Laboratory, POSTECH *)
(* {luscani, gla}@postech.ac.kr *)


module type FormulaSig = sig
  type atom 
  type t = Top | Bot | Atom of atom | Conj of t*t
         | Disj of t*t | Imp of t*t | Box of t | Dia of t 
  val parse : string -> t
  val print : t -> string
  val prec : t -> int
end;;

module Formula : FormulaSig = struct
  type atom = string
  type t = Top | Bot | Atom of atom | Conj of t*t
         | Disj of t*t | Imp of t*t | Box of t | Dia of t

  (* parser produce Def.t type AST which needs to be converted into Formula.t
		 The conversion is straight forward *)
  let rec convert f = 
    match f with
      Def.Top -> Top
    | Def.Bot -> Bot
    | Def.Atom s -> Atom s
    | Def.Conj (a, b) -> Conj (convert a, convert b)
    | Def.Disj (a, b) -> Disj (convert a, convert b)
    | Def.Imp  (a, b) -> Imp (convert a, convert b)
    | Def.Box a -> Box (convert a)
    | Def.Dia a -> Dia (convert a)
		| Def.Neg a -> Imp (convert a, Bot)

  (* parse string typed input to generate type t formula *)
  let parse s =
		let s = s ^ "\n" in 
    try
      let lexbuf = Lexing.from_string s in
        convert (Parser.main Lexer.token lexbuf)
    with Lexer.Eof -> exit 0

  (* simple printing function for testing *)
  let rec print f = 
    match f with
      Top -> Def.top
    | Bot -> Def.bot
    | Atom s -> s
    | Conj (f1,f2) -> 
        "(" ^ (print f1) ^ Def.conj ^ (print f2) ^ ")"
    | Disj (f1,f2) ->
        "(" ^ (print f1) ^ Def.disj ^ (print f2) ^ ")"
    | Imp (f1,f2) ->
        "(" ^ (print f1) ^ Def.imp ^ (print f2) ^ ")"
    | Box f1 -> "(" ^ Def.box ^ (print f1) ^ ")"
    | Dia f1 -> "(" ^ Def.dia ^ (print f1) ^ ")"

  (* operator precedence of the formula.
     It returns the precedence level of top level operator. *)
  let prec f =
    match f with
      Top -> 1
    | Bot -> 1
    | Atom p -> 1
    | Box f1 -> 2
    | Dia f1 -> 2
    | Conj (f1, f2) -> 3
    | Disj (f1, f2) -> 4
    | Imp (f1, f2) -> 5
end;;



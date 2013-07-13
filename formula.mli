(* This file is distributed under the terms of 
   the GNU Lesser General Public License Version 2.1 *)
(* Hyungchul Park, Sungwoo Park *)
(* Programming Language Laboratory, POSTECH *)
(* {luscani, gla}@postech.ac.kr *)


(* Formula defition *)
module type FormulaSig = sig
  type atom 
  type t = Top | Bot | Atom of atom | Conj of t*t
         | Disj of t*t | Imp of t*t | Box of t | Dia of t
  val parse : string -> t
  val print : t -> string
  (* operator precedence *)
  val prec : t -> int
end;;

module Formula : FormulaSig



(* This file is distributed under the terms of
   the GNU Lesser General Public License Version 2.1 *)
(* Hyungchul Park, Sungwoo Park *)
(* Programming Language Laboratory, POSTECH *)
(* {luscani, gla}@postech.ac.kr *)


(* To retrieve formula info by label number, we build LabelLookup.map. 
   We can get sign info and subformula relation from labellookup. *)

open Label
open Formula

module type LabelLookupSig = sig
  type tu
  type composite = Top | Bot | Atom
                 | Conj of Label.t*Label.t
		 | Disj of Label.t*Label.t
		 | Imp  of Label.t*Label.t
		 | Box  of Label.t*tu
		 | Dia  of Label.t*tu
  type sign_l1 = L1 | F1 | N1
  type sign_l2 = L2 | F2 | N2
  type sign = L of sign_l1*sign_l2 | LR of sign_l1*sign_l2 | R
  type map
 
  val empty : map
  val add : map -> (Label.t*composite*sign) -> map
  val find : map -> Label.t -> (composite*sign)
  val print_conn : map -> Label.t -> string

  val build : Formula.t -> Label.map -> map
  val print_map : Label.map -> map -> string 
end;;

module LabelLookup : LabelLookupSig



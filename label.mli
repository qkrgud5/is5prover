(* This file is distributed under the terms of
   the GNU Lesser General Public License Version 2.1 *)
(* Hyungchul Park, Sungwoo Park *)
(* Programming Language Laboratory, POSTECH *)
(* {luscani, gla}@postech.ac.kr *)


(* Label.t is a label which substitutes for formula.
   By replacing formula with label, we hide complex formula typed values
   and use label as identifier.  *)
open Formula

module type LabelSig = sig
  type t
  type map

  val compare : t -> t -> int
  val empty : map

  val add : map -> Formula.t -> map
  val find : map -> Formula.t -> t
  val iter : map -> 'a -> (t -> 'a -> 'a) -> 'a
  (* for debugging *)
  val print_map : map -> string
  (* output *)
  val print : t -> string
  val print_tex : Formula.t -> map -> string

  val labeling : Formula.t -> map
end;;

module Label : LabelSig



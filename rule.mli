(* This file is distributed under the terms of
   the GNU Lesser General Public License Version 2.1 *)
(* Hyungchul Park, Sungwoo Park *)
(* Programming Language Laboratory, POSTECH *)
(* {luscani, gla}@postech.ac.kr *)


(* DerivedRuleSig.t specifies details of derived rules *)

open Label
open Formula
open Labellookup

module type DerivedRuleSig = sig
  type ruleid = ID of (Label.t * int)
  type twig = Twig of Label.t
  type trunk = TrunkL1 of Label.t | TrunkL2 of Label.t | TrunkP of Label.t
  type t = Box of ((twig list) * trunk * Label.t)   (* TrunkL2, focus *)
         | Dia of ((twig list) * trunk * Label.t)   (* TrunkP, focus *)
         | Disj of ((twig list) * trunk * trunk * Label.t) (* TrunkL1, TrunkL1, focus *)
         | Init of ((twig list) * Label.t * Label.t)  (* focus, concl *)
         | Bot of ((twig list) * Label.t)   (* focus *)
  type map

  val empty : map
  val add : map -> (Label.t*t) -> map
  val find : map -> Label.t -> (ruleid*t) list
  val findi : map -> ruleid -> t

  val rule_gen : Label.map -> LabelLookup.map -> map
  val print_map : Label.map -> map -> string
end;;

module Rule : DerivedRuleSig


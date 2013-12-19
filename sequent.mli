(* This file is distributed under the terms of
   the GNU Lesser General Public License Version 2.1 *)
(* Hyungchul Park, Sungwoo Park *)
(* Programming Language Laboratory, POSTECH *)
(* {luscani, gla}@postech.ac.kr *)


(* type definition of sequent and its proof searching function *)

open Label
open Formula
open Labellookup
open Rule

module type SequentSig = sig
  type context_id = Global | Local | BoxR of (Label.t option*Label.t) | DiaL of Label.t
  type right_app = Top | Conj | Disj_a | Disj_b | Imp | Box 
                 | Dia_a | Dia_b of context_id
  type left_app  = L1 of Rule.ruleid * context_id 
                 | L3 of Rule.ruleid * context_id

  type bookset 
  type bookset_all 
  type context = (Label.t list) * context_id
  type frame = context list
  type sequent = frame * context * context * Label.t * bookset_all
  type proof = Left of left_app * (proof list) * (proof list) 
             | Right of right_app * (proof list)
             | Done | NoProof | NotYet
  type map

  val empty_bset : bookset
  val add_bset : bookset -> left_app -> bookset 
  val del_bset : bookset -> left_app -> bookset
  val find_bset : bookset -> left_app -> bool

  val create : Label.t -> sequent
  val print : sequent -> string
  val print_proof : sequent -> proof -> LabelLookup.map -> Rule.map -> string
  val get_call_cnt : unit -> int

  val update_bset_trunk : bookset_all -> Rule.t -> left_app -> bookset_all
  val update_bset_twig : bookset_all -> Rule.t -> left_app -> bookset_all
  val apply : sequent -> Rule.t -> left_app -> (sequent list*sequent list)
  val prove_fun : sequent -> LabelLookup.map -> Rule.map -> proof
  val set_debug : unit->unit
  val get_search_space : unit->int
end;;

module Sequent : SequentSig

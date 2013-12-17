(* This file is distributed under the terms of
   the GNU Lesser General Public License Version 2.1 *)
(* Hyungchul Park, Sungwoo Park *)
(* Programming Language Laboratory, POSTECH *)
(* {luscani, gla}@postech.ac.kr *)


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

module LabelLookup : LabelLookupSig = struct
  type tu = int
  type composite = Top | Bot | Atom
                 | Conj of Label.t*Label.t
		 | Disj of Label.t*Label.t
		 | Imp  of Label.t*Label.t
		 | Box  of Label.t*tu
		 | Dia  of Label.t*tu
  type sign_l1 = L1 | F1 | N1
  type sign_l2 = L2 | F2 | N2
  type sign = L of sign_l1*sign_l2 | LR of sign_l1*sign_l2 | R

  module MapLabel = Map.Make (Label)
  type map = (composite*sign) MapLabel.t

  (* sign F1 is subset of sign L1 since L1 can always be converted into F1 
     by signed subformula rules.  Here we represent a signe of both F1 and L1 as L1. *)
  let merge_sign_l1 a b =
    match a with
      L1 -> L1
    | F1 -> (match b with L1 -> L1 | _ -> F1)
    | N1 -> b

  let merge_sign_l2 a b =
    match a with
      L2 -> L2
    | F2 -> (match b with L2 -> L2 | _ -> F2)
    | N2 -> b

  (* call merge_sing when there already exists sign info in labellookup and
     we like to update with supset of sign *)
  let merge_sign a b =
    match a with
      R -> (match b with
              L (bsl1, bsl2) -> LR (bsl1, bsl2)
            | LR (bsl1, bsl2) -> LR (bsl1, bsl2)
            | R -> R)
    | LR (asl1, asl2) -> (match b with 
                            L (bsl1, bsl2) -> LR (merge_sign_l1 asl1 bsl1,
                                                  merge_sign_l2 asl2 bsl2)
                          | LR (bsl1, bsl2) -> LR (merge_sign_l1 asl1 bsl1,
                                                   merge_sign_l2 asl2 bsl2)
                          | R -> LR (asl1, asl2))
    | L (asl1, asl2) -> (match b with
                           L (bsl1, bsl2) -> L (merge_sign_l1 asl1 bsl1,
                                                merge_sign_l2 asl2 bsl2)
                         | LR (bsl1, bsl2) -> LR (merge_sign_l1 asl1 bsl1,
                                                  merge_sign_l2 asl2 bsl2)
                         | R -> LR (asl1, asl2))

  let empty = MapLabel.empty

  let add m (l, c, s) =
    try 
      let _, orig = MapLabel.find l m in
      let merged = merge_sign orig s in
        (MapLabel.add l (c, merged) m)
    with Not_found -> MapLabel.add l (c,s) m

  let find m l =
    MapLabel.find l m

  let print_conn m l =
    let c,s = find m l in
    match c with 
      Top -> "\\top"
    | Bot -> "\\bot"
    | Atom -> "init"
    | Conj (l1,l2) -> "\\wedge"
    | Disj (l1,l2) -> "\\vee"
    | Imp (l1,l2) -> "\\supset"
    | Box (l1,lu) -> "\\Box"
    | Dia (l1,lu) -> "\\Diamond"

  (* This build fun implements signed inference rules correspoding to each connective. *)
  let build f lmap =
    let uniq_cnt = ref 1 in
    let rec fun1 f s lmap lookup  =
      let l = Label.find lmap f in
      match f with
        Formula.Top -> add lookup (l, Top, s)
      | Formula.Bot -> add lookup (l, Bot, s)
      | Formula.Atom p -> add lookup (l, Atom, s)
      | Formula.Conj (f1, f2) ->
          let s1 =
            match s with
              L (sl1, sl2) ->  L ((if sl1=N1 then N1 else F1),
                                  (if sl2=N2 then N2 else F2))
            | LR (sl1, sl2) -> LR ((if sl1=N1 then N1 else F1),
                                   (if sl2=N2 then N2 else F2))
            | R -> R
          in
          let lookup1 = fun1 f1 s1 lmap lookup  in
          let lookup2 = fun1 f2 s1 lmap lookup1  in
          let c = Conj (Label.find lmap f1, Label.find lmap f2) in
            add lookup2 (l, c, s)
      | Formula.Disj (f1, f2) ->
          let s1 = 
            match s with    (* fix - wrong propagation of sign*)
              L (sl1, sl2) -> L (L1, N2)
            | LR (sl1, sl2) -> LR (L1, N2)
            | R -> R
          in
          let lookup1 = fun1 f1 s1 lmap lookup  in
          let lookup2 = fun1 f2 s1 lmap lookup1  in
          let c = Disj (Label.find lmap f1, Label.find lmap f2) in
            add lookup2 (l, c, s)
      | Formula.Imp (f1, f2) ->
          let s1 = 
            match s with 
              L (sl1, sl2) -> R
            | LR (sl1, sl2) -> LR (L1, N2)
            | R -> L (L1, N2)
          in
          let s2 = 
            match s with
              L (sl1, sl2) -> L ((if sl1=N1 then N1 else F1), 
                                 (if sl2=N2 then N2 else F2))
            | LR (sl1, sl2) -> LR ((if sl1=N1 then N1 else F1),
                                   (if sl2=N2 then N2 else F2))
            | R -> R
          in
          let lookup1 = fun1 f1 s1 lmap lookup in
          let lookup2 = fun1 f2 s2 lmap lookup1 in
          let c = Imp (Label.find lmap f1, Label.find lmap f2) in
            add lookup2 (l, c, s)
      | Formula.Box f1 ->
          let s1 = 
            match s with
              L (sl1, sl2) -> L (N1, L2)
            | LR (sl1, sl2) -> LR (N1, L2)
            | R -> R
          in
          let lookup1 = fun1 f1 s1 lmap lookup  in
          let c = Box (Label.find lmap f1, !uniq_cnt) in
          let _ = uniq_cnt := !uniq_cnt + 1 in
            add lookup1 (l, c, s)
      | Formula.Dia f1 ->
          let s1 =
            match s with
              L (sl1, sl2) -> L (L1, N2)
            | LR (sl1, sl2) -> LR (L1, N2)
            | R -> R
          in
          let lookup1 = fun1 f1 s1 lmap lookup  in
          let c = Dia (Label.find lmap f1, !uniq_cnt) in
          let _ = uniq_cnt := !uniq_cnt + 1 in
            add lookup1 (l, c, s)
    in
      fun1 f R lmap empty 

  let print_comp c = 
    match c with
      Top -> "true"
    | Bot -> "false"
    | Atom -> "Atom"
    | Conj (l1, l2) -> (Label.print l1) ^ "/\\" ^ (Label.print l2)
    | Disj (l1, l2) -> (Label.print l1) ^ "\\/" ^ (Label.print l2)
    | Imp  (l1, l2) -> (Label.print l1) ^ "->"  ^ (Label.print l2)
    | Box (l1, lu) -> "[]" ^ (Label.print l1)
    | Dia (l1, lu) -> "<>" ^ (Label.print l1)

  let print_sign_l1 sl1 =
    match sl1 with
    | L1 -> "- ~ "
    | F1 -> "~ "
    | N1 -> ""

  let print_sign_l2 sl2 =
    match sl2 with
    | L2 -> "= ~~ "
    | F2 -> "~~ "
    | N2 -> ""

  let print_sign s =
    match s with
    | L (sl1, sl2) -> "L " ^ (print_sign_l1 sl1) ^ (print_sign_l2 sl2)
    | LR (sl1, sl2) -> "LR + " ^ (print_sign_l1 sl1) ^ (print_sign_l2 sl2)
    | R -> "R +"

  let print_map lmap m =
    let fun1 l str = 
      let (c, s) = find m l in
      str ^ "\n" ^ (Label.print l) ^ " -> " ^ (print_comp c) ^ " " ^ (print_sign s)
    in
      Label.iter lmap "" fun1
 
end;;


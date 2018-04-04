(* This file is distributed under the terms of
   the GNU Lesser General Public License Version 2.1 *)
(* Hyungchul Park, Sungwoo Park *)
(* Programming Language Laboratory, POSTECH *)
(* {luscani, gla}@postech.ac.kr *)


open Label
open Formula
open Labellookup

module type DerivedRuleSig = sig
  type ruleid = ID of (Label.t * int)
  (* Label.t is the label of the goal formula in the twig sequent *)
  type twig = Twig of Label.t
  (* TrunkL1 : local context 
   * TrunkL2 : global context
   * TrunkP  : frame 
   * Label.t : subformula added to the context in the trunk *)
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

module Rule : DerivedRuleSig = struct
  (* Label.t : the principal formula of the conclusion of the derived rule
   * ID(l, n) : this rule is the n-th derived rule for l among the rules 
   * that have l as the principal formula *)
  type ruleid = ID of (Label.t * int)
  type twig = Twig of Label.t
  type trunk = TrunkL1 of Label.t | TrunkL2 of Label.t | TrunkP of Label.t
  type t = Box of ((twig list) * trunk * Label.t)   (* TrunkL2, focus *)
         | Dia of ((twig list) * trunk * Label.t)   (* TrunkP, focus *)
         | Disj of ((twig list) * trunk * trunk * Label.t) (* TrunkL1, TrunkL1, focus *)
         | Init of ((twig list) * Label.t * Label.t)  (* focus, concl *)
         | Bot of ((twig list) * Label.t)   (* focus *)

  module MapLabel = Map.Make (Label)
  type map = ((ruleid*t) list) MapLabel.t

  let empty = MapLabel.empty
  (* add : map -> Label.t * t -> map 
   * l is the principal formula of the conclusion of the derived rule. *)
  let add m (l, r) = 
    let orig_list = 
      try
        MapLabel.find l m
      with Not_found -> []
    in
    let id = ID (l, (List.length orig_list)) in
      MapLabel.add l ((id, r)::orig_list) m

  let find m l = try MapLabel.find l m
                 with Not_found -> []
  let findi m (ID (l, idx)) =
    let rulelist = find m l in
    let (id, rule) = List.nth rulelist ((List.length rulelist)-idx-1) in
      rule
  
  (* traverses the label tree structure in order to reach stoup introduction *)
  (* label_rec : Label.t -> Label.t -> twig list -> LabelLookup.map -> map -> rules 
   * l is the current principal formula.
   * base_l is the principal formula of the conclusion of the derived rule. *)
  let rec label_rec l base_l twigs lookup rules = 
    let c, s = LabelLookup.find lookup l in
      match c with
        LabelLookup.Top -> rules
      | LabelLookup.Bot -> 
        let rule = Bot (twigs, base_l) in
          add rules (base_l, rule)
      | LabelLookup.Atom ->
        let rule = Init (twigs, base_l, l) in
          (match s with
            LabelLookup.L (_,_) -> rules
          | _ -> add rules (base_l, rule))
      | LabelLookup.Conj (l1, l2) ->
        let rules1 = label_rec l1 base_l twigs lookup rules in
        let rules2 = label_rec l2 base_l twigs lookup rules1 in
          rules2
      | LabelLookup.Disj (l1, l2) ->
        let rule = Disj (twigs, TrunkL1 l1, TrunkL1 l2, base_l) in
          add rules (base_l, rule)
      | LabelLookup.Imp (l1, l2) ->
        let twigs1 = (Twig l1) :: twigs in
        let rules1 = label_rec l2 base_l twigs1 lookup rules in
          rules1
      | LabelLookup.Box (l1,lu) ->
        let rule = Box (twigs, TrunkL2 l1, base_l) in
          add rules (base_l, rule)
      | LabelLookup.Dia (l1,lu) ->
        let rule = Dia (twigs, TrunkP l1, base_l) in
          add rules (base_l, rule)
 
  (* the label l stands for the principal formula *)
  (* rule_gen_label : LabelLookup.map -> Label.t -> map -> map *)
  let rule_gen_label lookup l rules =
    let c, s = LabelLookup.find lookup l in
      let res =
      match s with
        LabelLookup.R -> rules
      | LabelLookup.L (sl1, sl2) ->
          if sl1=LabelLookup.L1 || sl2=LabelLookup.L2 then 
            (label_rec l l [] lookup rules)
          else rules
      | LabelLookup.LR (sl1, sl2) -> 
          if sl1=LabelLookup.L1 || sl2=LabelLookup.L2 then 
            (label_rec l l [] lookup rules)
          else rules
      in
        res

  (* rule_gen : Label.map -> LabelLookup.map -> map *) 
  let rule_gen lmap lookup =
    Label.iter lmap empty (rule_gen_label lookup)

  let print_twig (Twig l) =
    "\\seqc{}{}{}{\\bf L_{" ^ (Label.print l) ^ "}}"
  let print_trunk trunk_premise =
    match trunk_premise with
      TrunkL1 l1 -> "\\seqc{}{}{, {\\bf L_{" ^ (Label.print l1) ^ "}}}{M}"
    | TrunkL2 l1 -> "\\seqc{}{, {\\bf L_{" ^ (Label.print l1) ^ "}}}{}{M}"
    | TrunkP l1 -> "\\seqc{;{\\bf L_{" ^ (Label.print l1) ^ "}}}{}{}{M}"

  (* label is the principal formula of the conclusion sequent *)
  let print_twig2 label (Twig l) =
    "\\seqc{}{}{, " ^ label ^ "}{\\bf L_{" ^ (Label.print l) ^ "}}"
  let print_trunk2 label trunk_premise =
    match trunk_premise with
      TrunkL1 l1 -> "\\seqc{}{}{, " ^ label ^ ", {\\bf L_{" ^ (Label.print l1) ^ "}}}{M}"
    | TrunkL2 l1 -> "\\seqc{}{, {\\bf L_{" ^ (Label.print l1) ^ "}}}{, " ^ label ^ "}{M}"
    | TrunkP l1 -> "\\seqc{;{\\bf L_{" ^ (Label.print l1) ^ "}}}{}{, " ^ label ^ "}{M}"

  let twigs_of_rule rule =
    match rule with
      Box (twigs, _, _) -> twigs
    | Dia (twigs, _, _) -> twigs
    | Disj (twigs, _, _, _) -> twigs
    | Init (twigs, _, _) -> twigs
    | Bot (twigs, _) -> twigs

  let principal_of_rule rule =
    match rule with
      Box (_, _, l) -> l
    | Dia (_, _, l) -> l
    | Disj (_, _, _, l) -> l
    | Init (_, l, _) -> l
    | Bot (_, l) -> l

  let quad_join s1 s2 = 
    if s1 = "" && s2 = "" then ""
    else if s1 = "" then s2
    else if s2 = "" then s1
    else s1 ^ " \\quad " ^ s2

  (* IM: the principal formula of the conclusion sequent should also appear in the premises *)
  let print_rule rule =
    (* the principal formula of the conclusion sequent *)
    let label_str = "L_{" ^ (Label.print (principal_of_rule rule)) ^ "}" in
    let twigs = twigs_of_rule rule in
    (* let twigs_str = List.fold_left (fun x y -> x ^ (print_twig y)) "" twigs in *)
    let twigs_str = List.map (print_twig2 label_str) twigs in
    let twigs_str = String.concat " \\quad " twigs_str in
    let twigs_str = if twigs_str = "" then ""
                    else ("\\overbrace{" ^ twigs_str ^ "}^{\\text{twigs}}") in
    let trunk_str =
      match rule with
        Box (_, t1, _) -> "\\overbrace{" ^ (print_trunk2 label_str t1) ^ "}^{\\text{trunk}}"
      | Dia (_, t1, _) -> "\\overbrace{" ^ (print_trunk2 label_str t1) ^ "}^{\\text{trunk}}"
      | Disj (_, t1, t2, _) ->
        "\\overbrace{" ^
        (quad_join (print_trunk2 label_str t1) (print_trunk2 label_str t2)) ^
        "}^{\\text{trunks}}"
      | Init (_, _, _) -> ""
      | Bot (_, _) -> ""
    in
    let concl_str = 
      let label_str = "L_{" ^ (Label.print (principal_of_rule rule)) ^ "}" in
      match rule with
        Init (_, l, latom) ->
        "\\seqc{}{}{, {\\bf " ^ label_str ^ "}}{L_{" ^ (Label.print latom) ^ "}}"
      | _ -> "\\seqc{}{}{, {\\bf " ^ label_str ^ "}}{M}"
    in
      "$$\n\\infer {" ^ concl_str ^ "}{" ^ (quad_join twigs_str trunk_str) ^ "}\n$$\n"

  let print_map lmap m =
    let print_rules m l str =
      let rules = try find m l with Not_found -> [] in
        str ^ (List.fold_left (fun x (_,y) -> x ^ (print_rule y)) "" rules)
    in
    Label.iter lmap "" (print_rules m)
end;;

(*
 
let str = "<>(<>a -> (b\\/c)) /\\ a -> <>b \\/ <>c\n";;
let f = Formula.parse str;;
let lmap = Label.labeling f;;
let m = LabelLookup.build f lmap;;
let rules = Rule.rule_gen lmap m;;

print_string (Label.print_map lmap);;

print_string (Label.print_tex f lmap);
print_string "\n";;

print_string (LabelLookup.print_map lmap m);
print_string "\n";;

print_string (Rule.print_map lmap rules);
print_string "\n";;

*)

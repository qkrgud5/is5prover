(* This file is distributed under the terms of
   the GNU Lesser General Public License Version 2.1 *)
(* Hyungchul Park, Sungwoo Park *)
(* Programming Language Laboratory, POSTECH *)
(* {luscani, gla}@postech.ac.kr *)


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
  val get_search_space : unit -> int
end;;

module Sequent : SequentSig = struct
  type context_id = Global | Local | BoxR of (Label.t option*Label.t) | DiaL of Label.t
  type right_app = Top | Conj | Disj_a | Disj_b | Imp | Box 
                 | Dia_a | Dia_b of context_id
  type left_app  = L1 of Rule.ruleid * context_id 
                 | L3 of Rule.ruleid * context_id
  type rule_app = Rule.ruleid * context_id 
  type goal_id = Label.t * context_id
  
  let print_ctxt_id ctxt_id =
    match ctxt_id with
      Global -> "g"
    | Local -> "l"
    | BoxR (Some lastboxl, label) -> (Label.print lastboxl)^","^(Label.print label)
    | BoxR (None, label) -> "\\text{n},"^(Label.print label)
    | DiaL label -> Label.print label;;

  module RuleApp = struct
    type t = rule_app
    let compare = compare
		let print ((Rule.ID (label, idx)), ctxt_id) = 
			let str = Printf.sprintf "app_id (%s, %d) ctxt %s" (Label.print label) idx (print_ctxt_id ctxt_id) in
			  str
  end

  module Goal_id = struct
    type t = goal_id
    let compare = compare
  end

  module BSet = Set.Make (RuleApp)
  module BSetR = Set.Make (Label)
  module BSetG = Set.Make (Goal_id)

  type bookset = BSet.t
  type bookset_all = BSet.t * BSet.t * BSet.t * BSet.t 
                     * BSet.t * BSet.t * BSet.t * BSetR.t * BSetR.t
                     * (Label.t option) * BSetG.t * BSetG.t
  type context = (Label.t list) * context_id
  type frame = context list
  type sequent = frame * context * context * Label.t * bookset_all
  type proof = Left of left_app * (proof list) * (proof list) 
             | Right of right_app * (proof list)
             | Done | NoProof | NotYet

  module MapDigest = Map.Make (Digest)
  type map = (proof) MapDigest.t

  (* debug switch *)
  let debug = ref false;;
  let set_debug _ = debug:=true;;

  let rule_app_from_left l = 
    match l with L1 (a,b) -> (a,b) | L3 (a,b) -> (a,b);;

  (* single bset operation *)
  let empty_bset = BSet.empty;;
  let empty_bsetr = BSetR.empty;;
  let empty_bsetg = BSetG.empty;;
  let add_bset s l = BSet.add (rule_app_from_left l) s;;
  let del_bset s l = BSet.diff s (BSet.add (rule_app_from_left l) empty_bset);;
  let find_bset s l = BSet.mem (rule_app_from_left l) s;;
  let union_bset s1 s2 = BSet.union s1 s2;;
  let diff_bset s1 s2 = BSet.diff s1 s2;;
  let proper_sub_bset s1 s2 = (BSet.subset s1 s2) & ((BSet.cardinal s1) < (BSet.cardinal s2));;

  (* create sequnet from a single label *)
  let create l = ([], ([], Global), ([], Local), l,
                  (empty_bset, empty_bset, empty_bset, empty_bset, empty_bset,
                   empty_bset, empty_bset, empty_bsetr, empty_bsetr, None,
                   empty_bsetg, empty_bsetg));;

  let print_label label =
    "L_{" ^ (Label.print label) ^ "}";;

  let print_conn rule =
    match rule with
      Rule.Box _ -> "\\Box" | Rule.Dia _ -> "\\Diamond" | Rule.Disj _ -> "\\vee" 
    | Rule.Init _ -> "init" | Rule.Bot _ -> "\\bot";;

  let print_left rule_map lapp =
    match lapp with
      L1 (ruleid, ctxt_id) ->
        let Rule.ID (rule_label, idx) = ruleid in
        let rule = Rule.findi rule_map ruleid in
        let conn = print_conn rule in
          conn ^ " L_a (" ^ (print_label rule_label) ^ "," ^ string_of_int(idx)  ^ "," ^ (print_ctxt_id ctxt_id) ^ ")"
    | L3 (ruleid, ctxt_id) ->
        let Rule.ID (rule_label, idx) = ruleid in
        let rule = Rule.findi rule_map ruleid in
        let conn = print_conn rule in
          conn ^ " L_b (" ^ (print_label rule_label) ^ "," ^ string_of_int(idx)  ^ "," ^ (print_ctxt_id ctxt_id) ^ ")";;

  let print_ctxt (llist, ctxtid) =
    let str_list = 
      match llist with
      | [] -> ""
      | h::[] -> print_label h 
      | h::t -> (print_label h) ^
                (List.fold_left (fun x y -> x^","^(print_label y)) "" t)  
    in
      Printf.sprintf "\\{%s\\}_{%s}" str_list (print_ctxt_id ctxtid);;

  let print_frame ctxt_list =
    match ctxt_list with
    | [] -> ""
    | h::[] -> print_ctxt h
    | h::t -> (print_ctxt h) ^
              (List.fold_left (fun str ctxt -> str^";"^(print_ctxt ctxt)) "" t);;

  (* print sequent *)
  let print (f, g, l, c, bset_all) = 
    let f1 str = if str="" then " \\ " else str in 
    "\\seq{" ^ (f1 (print_frame f)) ^ "}{" ^ (f1 (print_ctxt g)) ^ "}{" ^ 
    (f1 (print_ctxt l)) ^ "}{" ^ (f1 (print_label c)) ^ "}" ;;

  (* modify sequent *)
  let repl_concl (f,g,l,_,bset_all) c = (f,g,l,c,bset_all);;

  let add_to_local (f,g,(local_list, local_id),c,bset_all) l =
    (f,g,(l::local_list,local_id),c,bset_all);;

  let boxr_empty_local (f,g,l,c,bset_all) lastBoxL label_uniq =
    let local_list, _ = l in
    let new_f = if local_list=[] then f else (l::f) in
      (* phc - test : no lastBoxL *)
      (new_f,g,([], BoxR (lastBoxL, label_uniq)),c,bset_all);; 
    (*  (new_f,g,([], BoxR (None, label)),c,bset_all) ;; *)

  (* returns all the sequent which exchanged local context with an accessible context *)    
  let switch_ctxt_all (f,g,l,c,bset_all) =
    let rec fun1 f1 (f2,g,l,c,bset_all) =
      match f2 with
        [] -> []
      | h::t -> 
        let new_seq = (List.concat [f1;(l::t)],g,h,c,bset_all) in
          new_seq::(fun1 (h::f1) (t,g,l,c,bset_all))
    in
      fun1 [] (f,g,l,c,bset_all);;
  
  let enum_pair l = 
    let rec fun1 l count = 
      match l with
        [] -> []
      | h::t -> (h,count) :: (fun1 t (count+1)) 
  in
    fun1 l 0;;

  (* repeat concatenating n copies of pat string *) 
  let repeat pat n =
    let rec fun1 str pat n =
      if n=0 then str else (fun1 (str^pat) pat (n-1))
    in
      fun1 "" pat n;;

  (* raise MaxDepth when the call depth of prove fun is over the limit *)
  exception MaxDepth

  (* debug_cnt simply counts the number debug_print *) 
	let debug_cnt = ref 0;;
  let call_level = ref 0;;
  let debug_print str = 
    if !debug then
			let _ = debug_cnt := !debug_cnt + 1 in
      (* let _ = (if !debug_cnt mod 1=0 then (input_line stdin) else "") in *)
      (print_string ((repeat " " !call_level)^"|"); print_string str;
       print_newline ())
    else ();;

  let max_depth = ref 0;;
  (* call depth counts the number of call frame
     some of prove fun call may exit without doing anything (due to redun)
     in that case, we don't count max_depth alive.
     max_depth_alive is practical call depth used for coverage test *)
  let max_depth_alive = ref 0;;
  let depth_history = ref [];;
  let branch_history = ref [];;
  let map_seq_digest = ref (MapDigest.empty);;
  let get_search_space () = 
    let count = ref 0 in
    let fun1 _ _ = 
      count := !count + 1; true
    in
    let _ =  MapDigest.for_all fun1 !map_seq_digest in
      !count

  let seq_to_digest seq = Digest.string (print seq);;

  (* call_level means the current number of prove fun call frames
     max value of call_cnt updates max_depth and max_depth_alive *)
  let call_cnt = ref 0;;
  let call_reset _ = 
    debug_cnt:=0;  call_level:=0;
    call_cnt:=0; depth_history:=[]; branch_history:=[];
    map_seq_digest := MapDigest.empty;;
    max_depth:=0; max_depth_alive:=0; ();;
  let call_mark _ = 
    call_level:=!call_level+1;
    call_cnt:=!call_cnt+1;
    if !call_level > !max_depth then begin
      max_depth := !call_level;
      debug_print ("======================================");
      debug_print ("max_depth" ^ (string_of_int !call_level));
      debug_print ("======================================") end
    else ();
    if !max_depth > 1000 then begin
      debug_print ("======================================");
      debug_print ("max_depth reaches limit, start backtrace");
      debug_print ("======================================");
      raise MaxDepth
    end else ();;

  let call_mark_ret  _ = call_level:=!call_level-1;;

  (* formulas of seq1 are covered by those of seq2 *)
  (* If we have seq2 in history, then give up seq1 *)
  let test_incl seq1 seq2 =
    let (f1,(g1,_),(l1, _),c1,bset1) = seq1 in
		let (f2,(g2,_),(l2, _),c2,bset2) = seq2 in
    let ctx_incl l1 l2 = List.for_all (fun ll1 -> List.exists (fun ll2 -> ll1=ll2) l2) l1 in
      if c1!=c2 then false
      else if ctx_incl g1 g2 = false then false
      else if ctx_incl l1 (g2@l2) = false then false
      else if (List.for_all (fun (ff1,_) -> List.exists (fun (ff2,_) -> ctx_incl ff1 (g2@ff2)) f2) f1)=false then false
      else true;;

  (* None : no match found in the history
     Some Done : match found and already proven
     Some NoProof : match found and already refuted
     Some NotYet : match found and the sequent is not yet completed
                   that is, the sequent found is one of current seq's parents *)
  let test_in_history seq =
(*    let _ = debug_print "test_in_history > " in
    let _ = debug_print (print seq) in *)
    let res = List.fold_left (fun last (seqh, closed) -> 
(*      let _ = debug_print (match closed with NotYet -> "NotYet" | NoProof -> "NoProof" | Done -> "Done"); debug_print " : "; debug_print (print seqh) in *)
      match last with 
      | None -> begin match closed with 
                | NotYet -> None
                | _ -> if test_incl seqh seq then Some closed else None end
      | Some Done | Some NoProof -> last
      | _ -> print_endline "history error"; None) None !depth_history 
    in
(*    let _ = debug_print "test_in_history < " in *)
      res;;

  let test_parents seq =
    let rec fun_parents seq parents =
      match parents with
      | [] -> false 
      | pseq :: tl -> if (test_incl seq pseq) then true else (fun_parents seq tl)
    in
      fun_parents seq (List.tl !branch_history)

  let get_call_cnt _ = !call_cnt;;
  exception ProverError of string

  (* list operation used to change frame contexts of sequent *)
  let rec find_first l f str =
    match l with
      [] -> let _ = debug_print str in raise (ProverError "find_first failed")
    | hl::tl -> if f hl then hl else (find_first tl f str);;
  
  let rec update_first l f1 f2 str =
    match l with
      [] -> let _ = debug_print str in raise (ProverError "update_first failed")
    | hl::tl -> if (f1 hl) then (f2 hl)::tl else (update_first l f1 f2 str) ;;
  
  let rec remove_first l f str =
    match l with
      [] -> let _ = debug_print str in raise (ProverError "remove_first failed") 
    | hl::tl -> if (f hl) then tl else hl::(remove_first tl f str);;

  let rule_app_to_label (Rule.ID (label,_), _) = label;;
  let left_app_to_label lapp =
    match lapp with
      (L1 (Rule.ID (label,_), _)) -> label
    | (L3 (Rule.ID (label,_), _)) -> label;;

	let print_bset set1 = 
		let _ = BSet.iter (fun x -> print_string (RuleApp.print x); print_string ",") set1; print_newline () in ();;

	let print_bsetr set1 =
    let _ = BSetR.iter (fun x -> print_string (Label.print x); print_string "," ) set1; print_newline () in ();;
	
  let print_bsetg set1 =
    let _ = BSetG.iter (fun (x, ctx_id) -> print_string (Label.print x); print_string ","; print_string (print_ctxt_id ctx_id); print_string "; ") set1; print_newline () in ();;


  let print_bset_all bset_all = 
	 if !debug then
	  let boxr, diar, disjr, botr, initr, boxt,
        diat, boxR, diaR,_,_,pset = bset_all in
  		print_string "boxr  : "; print_bset boxr;
  		print_string "diar  : "; print_bset diar;
  		print_string "disjr : "; print_bset disjr;
  		print_string "botr  : "; print_bset botr;
  		print_string "initr : "; print_bset initr;
  		print_string "boxt  : "; print_bset boxt;
  		print_string "diat  : "; print_bset diat;
  		print_string "boxR  : "; print_bsetr boxR;
  		print_string "diaR  : "; print_bsetr diaR;
  		print_string "pset  : "; print_bsetg pset
   else ()
	
  (* update bookkeeping set *)
  let update_bset_trunk bset_all rule lapp =
    let boxr, diar, disjr, botr, initr, boxt, diat, boxR, diaR, lastBoxL, goalset, pset = bset_all in
    match rule with
      Rule.Box p -> (add_bset (diff_bset boxr boxt) lapp, diar,
                     empty_bset, empty_bset, empty_bset,
                     empty_bset, empty_bset, empty_bsetr, empty_bsetr, Some (left_app_to_label lapp), goalset, empty_bsetg)
    | Rule.Dia p -> (diff_bset boxr boxt, add_bset (diff_bset diar diat) lapp,
                     empty_bset, empty_bset, empty_bset,
                     empty_bset, empty_bset, boxR, empty_bsetr, lastBoxL, goalset, empty_bsetg)
    | Rule.Disj p -> (diff_bset boxr boxt, diff_bset diar diat,
                      add_bset disjr lapp, empty_bset, empty_bset,
                      empty_bset, empty_bset, boxR, diaR, lastBoxL, goalset, empty_bsetg)
    | Rule.Bot p -> bset_all
    | Rule.Init p -> bset_all;;

  let update_bset_twig  bset_all rule lapp =
    let boxr, diar, disjr, botr, initr, boxt, diat, boxR, diaR, lastBoxL, goalset, pset = bset_all in
    match rule with
      Rule.Box p -> (add_bset boxr lapp, diar, disjr, botr, initr,
                     add_bset boxt lapp, diat, boxR, diaR, lastBoxL, goalset, pset)
    | Rule.Dia p -> (boxr, add_bset diar lapp, disjr, botr, initr,
                     boxt, add_bset diat lapp, boxR, diaR, lastBoxL, goalset, pset)
    | Rule.Disj p -> (boxr, diar, add_bset disjr lapp, botr, initr,
                      boxt, diat, boxR, diaR, lastBoxL, goalset, pset)
    | Rule.Bot p ->  (boxr, diar, disjr, add_bset botr lapp, initr,
                     boxt, diat, boxR, diaR, lastBoxL, goalset, pset) 
    | Rule.Init p -> (boxr, diar, disjr, botr, add_bset initr lapp, 
                      boxt, diat, boxR, diaR, lastBoxL, goalset, pset);;

  (* is this rule applicable to the sequent *) 
  let applicable seq rule lapp =
    (* let _ = if debug then print_string "applicable\n" else () in *)
    let (f, g, l, c, bset_all) = seq in
    let boxr, diar, disjr, botr, initr, boxt, diat, boxR, diaR, lastBoxL, goalset, pset = bset_all in
    let the_bset =
      match rule with Rule.Box p -> boxr | Rule.Dia p -> diar
      | Rule.Disj p -> disjr | Rule.Bot p -> botr | Rule.Init p -> initr 
    in 
    let the_ctxt = 
      match lapp with
        L1 (rid, ctxt_id) -> l
      | L3 (rid, ctxt_id) -> 
        find_first f (fun (l1, ctxt_id1) -> ctxt_id = ctxt_id1) "applicable_the_ctxt"
    in
    let trunk_redun (g_ctxt_list, _) (the_ctxt_list, _) =
      match rule with
        Rule.Box (_, Rule.TrunkL2 newl, _) -> List.mem newl g_ctxt_list
      | Rule.Dia (_, Rule.TrunkP newl, focusl) ->
          (* let _ = if debug then print_string "Rule.Dia\n" else () in*)
          let fun1 b (ctxt_list, ctxt_id) = 
            if b then true
            else if DiaL focusl = ctxt_id then true 
            else false 
          in
          List.fold_left fun1 false (l::f)
      | Rule.Disj (_, Rule.TrunkL1 newl1, Rule.TrunkL1 newl2, _) -> 
        (List.mem newl1 the_ctxt_list) || (List.mem newl2 the_ctxt_list)
      | Rule.Init _ -> false
      | Rule.Bot _ -> false
      | _ -> raise (ProverError "redundancy check error")
    in 
    let test_etc =
      match rule with
        Rule.Init (_, _, concl) -> 
          (match lapp with L3 (_,_) -> false | L1 (_,_) -> concl=c)
      | _ -> true 
    in
      if (find_bset the_bset lapp) then false
      else (if trunk_redun g the_ctxt then false else test_etc);;
    
  let add_to_global (f, (gctxt_list, gctxt_id), l, c, bset_all) newl =
    (f, (newl::gctxt_list, gctxt_id), l, c, bset_all);;
  let add_to_frame (f, g, l, c, bset_all) new_ctxt =
    (new_ctxt::f, g, l, c, bset_all);;
  let add_to_ctxt (f, g, l, c, bset_all) newl lapp =
    match lapp with
      L1 (_, _) -> 
        let (local_list, local_id) = l in
          (f, g, (newl::local_list, local_id), c, bset_all)
    | L3 (_, ctxt_id) ->
        (update_first f (fun (ctxt_list, ctxt_id1) -> ctxt_id1=ctxt_id)
           (fun (ctxt_list, ctxt_id1) -> (newl::ctxt_list, ctxt_id1)) "add_to_ctxt",
         g, l, c, bset_all);;

  let switch_ctxt seq ctxt_id = 
    let (f,g,l,c,bset_all) = seq in
    let local_list, _ = l in
    let new_f = (remove_first f (fun (_,ctxt_id1) -> ctxt_id1=ctxt_id)) "switch_remove" in
    let new_f = if local_list=[] then new_f else l::new_f in
    let new_l = find_first f (fun (_,ctxt_id1) -> ctxt_id1=ctxt_id) "switch_find" in
    (new_f, g, new_l, c, bset_all);;

  let rec gen_twigs seq twigs =
    let (f, g, l, c, bset_all) = seq in
    match twigs with
      [] -> []
    | (Rule.Twig hl)::tl -> (f,g,l,hl,bset_all)::(gen_twigs seq tl);;

  exception Sequent_error

  (* to reconstruct proof tree after we found a complete one *)
  let apply_noset seq rule lapp = 
    (* let _ = if !debug then Printf.printf "apply_noset %s\n" (print seq) else () in*)
    let (f,g,l,c,bset_all) = seq in
    let seq_twig = 
      match lapp with
        L1 (_, _) -> (f, g, l, c, bset_all)
      | L3 (_, ctxt_id) -> switch_ctxt seq ctxt_id
    in
    match rule with
      Rule.Box (twigs, Rule.TrunkL2 newl, _) -> 
        let trunk1 = (add_to_global seq newl) in
          ([trunk1],(gen_twigs seq_twig twigs))
    | Rule.Dia (twigs, Rule.TrunkP newl, focusl) ->
        let trunk1 = (add_to_frame seq ([newl], DiaL focusl)) in
          ([trunk1],(gen_twigs seq_twig twigs))
    | Rule.Disj (twigs, Rule.TrunkL1 newl1, Rule.TrunkL1 newl2, _) ->
        let trunk1 = (add_to_ctxt seq newl1 lapp) in
        let trunk2 = (add_to_ctxt seq newl2 lapp) in
          ([trunk1;trunk2],(gen_twigs seq_twig twigs))
    | Rule.Init (twigs, _, _) -> ([],gen_twigs seq_twig twigs)
    | Rule.Bot (twigs, _) -> ([],gen_twigs seq_twig twigs)
    | _ -> raise Sequent_error

	exception Bset_termination of string 
	exception Bset_OK

  (* testing code *) 
  (* 2 : premise , 1 : conclusion *)
  let bset_test seq1 seq2 = 
    let (f1,g1,l1,c1,bset1) = seq1 in
		let (f2,g2,l2,c2,bset2) = seq2 in
   let bset_test_in seq1 seq2 =
    let (f1,g1,l1,c1,bset1) = seq1 in
		let (f2,g2,l2,c2,bset2) = seq2 in
  	let test conclusion premise reason =
		  if proper_sub_bset premise conclusion 
			then (print_string "set_of_concl : \n"; print_bset conclusion;
				print_string "set_of_premise : \n"; print_bset premise;
				raise (Bset_termination reason))
			else (if BSet.equal conclusion premise then () else raise Bset_OK) in
		let test_ctxt : frame -> frame -> unit = 
			fun l1 l2 ->
      let fun1 (ctxt_list1,ctxt_id1) (ctxt_list2,ctxt_id2) =
        if ctxt_list1==[] then true else 
				if ctxt_id1!=ctxt_id2 then false else 
			  List.fold_left (fun x y -> x && (List.mem y ctxt_list2)) true ctxt_list1
			in
			let fun2 ctxt1 l2 =
				List.fold_left (fun x y -> x || (fun1 ctxt1 y)) false l2
			in
			let fun3 l1 l2 =
				List.fold_left (fun x y -> x && (fun2 y l2)) true l1
			in
				if (fun3 l1 l2) then () else (raise (Bset_termination "context"))
		in
	  let boxr1, diar1, disjr1, botr1, initr1, boxt1, diat1,boxR1,diaR1,_,_,_ = bset1 in
    let boxr2, diar2, disjr2, botr2, initr2, boxt2, diat2,boxR2,diaR2,_,_,_ = bset2 in
		let box1 = diff_bset boxr1 boxt1 in
		let box2 = diff_bset boxr2 boxt2 in
		let _ = test box1 box2 "box_union" in
		let dia1 = diff_bset diar1 diat1 in
		let dia2 = diff_bset diar2 diat2 in
		let _ = test dia1 dia2 "dia_union" in
		let _ = test_ctxt (l1::f1) (l2::f2) in
		let _ = if BSetR.equal boxR2 boxR1 then ()
						else (if (BSetR.subset boxR2 boxR1) then raise (Bset_termination "boxR") else ()) in
		let _ = if BSetR.equal diaR2 diaR1 then ()
						else (if (BSetR.subset diaR2 diaR1) then raise (Bset_termination "diaR") else ()) in
		let _ = try (test_ctxt (l2::f2) (l1::f1)) 
						with Bset_termination reason -> raise Bset_OK in
    let _ = test boxr1 boxr2 "box" in
		let _ = test diar1 diar2 "dia" in
		let _ = test disjr1 disjr2 "disj" in
		let _ = test botr1 botr2 "bot" in
		let _ = test initr1 initr2 "init" in
			()
		in
			try (bset_test_in seq1 seq2) 
			with Bset_OK -> ()
			| (Bset_termination reason) ->
        print_endline reason;
        print_endline (print seq1);
        print_endline (print seq2);
				print_string "concl ------------\n"; print_bset_all bset1;
				print_string "premi ------------\n"; print_bset_all bset2;
					raise (Bset_termination reason)

  (* find the principal formula and remove it *)
  let remove_in_ctx (label_list, ctx_id) label =
    let rec remove_in_list l r a =
      match l with
        [] -> [] (* in global context *)
      | hd::tl -> if hd==a then tl@l else remove_in_list tl (hd::r) a 
    in
      (remove_in_list label_list [] label, ctx_id)

  let remove_in_frame ctx_list ctx_id label =
    let rec find_in_frame ctx_list ctx_id label res =
      match ctx_list with
        [] -> [] (* in global context *)
      | (ll, it_ctx_id)::tl -> if it_ctx_id==ctx_id 
                               then (remove_in_ctx (ll, ctx_id) label)::res@tl 
                               else find_in_frame tl ctx_id label ((ll,it_ctx_id)::res)
    in
      find_in_frame ctx_list ctx_id label []

  let apply seq rule lapp =
    let (f, g, l, c, bset_all) = seq in
(*    let l = match lapp with L1 (Rule.ID (label,_),_)  -> remove_in_ctxt l label | _ -> l in *)
(*    let f = match lapp with L3 (Rule.ID (label,_),ctx_id) -> remove_in_frame f ctx_id label | _ -> f in *)
		let res_trunk, res_twig = 
      let twig_bset_all = update_bset_twig bset_all rule lapp in
      let trunk_bset_all = update_bset_trunk bset_all rule lapp in
      let seq_twig : sequent = 
        match lapp with
          L1 (_, _) -> (f, g, l, c, twig_bset_all)
        | L3 (_, ctxt_id) -> switch_ctxt (f,g,l,c,twig_bset_all) ctxt_id
      in
      let seq_trunk : sequent = (f, g, l, c, trunk_bset_all) in
      match rule with
        Rule.Box (twigs, Rule.TrunkL2 newl, _) -> 
          let trunk1 = (add_to_global seq_trunk newl) in
            ([trunk1],(gen_twigs seq_twig twigs))
      | Rule.Dia (twigs, Rule.TrunkP newl, focusl) ->
          let trunk1 = (add_to_frame seq_trunk ([newl], DiaL focusl)) in
            ([trunk1],(gen_twigs seq_twig twigs))
      | Rule.Disj (twigs, Rule.TrunkL1 newl1, Rule.TrunkL1 newl2, _) ->
          let trunk1 = (add_to_ctxt seq_trunk newl1 lapp) in
          let trunk2 = (add_to_ctxt seq_trunk newl2 lapp) in
            ([trunk1;trunk2],(gen_twigs seq_twig twigs))
      | Rule.Init (twigs, _, _) -> ([],gen_twigs seq_twig twigs)
      | Rule.Bot (twigs, _) -> 
					([],gen_twigs seq_twig twigs)
      | _ -> raise Sequent_error
		in
			(res_trunk, res_twig) 

  let update_seq_bset_box_r seq label = 
		let (f,g,l,c,bset_all) = seq in
    let boxr, diar, disjr, botr, initr, boxt, diat, boxR, diaR, lastBoxL, goalset, pset = bset_all in
		let new_bset_all = 
			  (diff_bset boxr boxt, diff_bset diar diat, 
				 empty_bset, empty_bset, empty_bset, empty_bset, empty_bset,
				 BSetR.add label boxR, diaR, lastBoxL, goalset, pset)
		in
			(f,g,l,c,new_bset_all)

  let update_seq_bset_dia_r2 seq label = 
		let (f,g,l,c,bset_all) = seq in
    let boxr, diar, disjr, botr, initr, boxt, diat, boxR, diaR, lastBoxL, goalset, pset= bset_all in
		let new_bset_all = 
			  (diff_bset boxr boxt, diff_bset diar diat, 
				 empty_bset, empty_bset, empty_bset, empty_bset, empty_bset,
				 boxR, BSetR.add label diaR, lastBoxL, goalset, BSetG.empty)
		in
			(f,g,l,c,new_bset_all)

  let update_seq_bset_imp_r2 seq = 
		let (f,g,l,c,bset_all) = seq in
    let boxr, diar, disjr, botr, initr, boxt, diat, boxR, diaR, lastBoxL, goalset, pset = bset_all in
		let new_bset_all = 
			  (diff_bset boxr boxt, diff_bset diar diat, 
				 empty_bset, empty_bset, empty_bset, empty_bset, empty_bset,
				 boxR, diaR, lastBoxL, goalset, BSetG.empty)
		in
			(f,g,l,c,new_bset_all)

  let goal_id_redun local_id c goalset =
    BSetG.mem (c, local_id) goalset 

  exception LevelError 

  (* main proof search function *)
  let prove_fun seq lookup rule_map = 
    let _ = call_reset () in
    let _ = debug_print "start prove_fun" in
    let rec prove : sequent -> proof = 
      fun seq -> begin
      
      let test_level = !call_level in
      let (f,g,l,c,bset_all) = seq in
      let (local_list, local_id) = l in
      let (global_list, _) = g in
      let boxr, diar, disjr, botr, initr, boxt, diat, boxR, diaR, lastBoxL, goalset, pset = bset_all in
      (* phc - another begin end enclosure, be cautious! *)
      if BSetG.mem (c, local_id) pset then NoProof else begin

      let pset = BSetG.add (c,local_id) pset in
      let bset_all = (boxr, diar, disjr, botr, initr, boxt, diat, boxR, diaR, lastBoxL, goalset, pset) in
      let seq = (f,g,l,c,bset_all) in
      
      let rec prove_disj_list l fun1 =
        match l with
          [] -> NoProof 
        | h::t -> (function NoProof -> (prove_disj_list t fun1) | p -> p) (fun1 h)
      in

      let rec prove_conj_list l fun1 =
        match l with
          [] -> []
        | h::t -> (fun1 h)::(prove_conj_list t fun1)
      in
  
      let prove_right _ =
        let _ = debug_print "> prove_right" in
        let comp, sign = LabelLookup.find lookup c in
        match comp with
          LabelLookup.Top -> Right (Top, [])
        | LabelLookup.Bot -> NoProof 
        | LabelLookup.Atom -> NoProof
        | LabelLookup.Conj (l1, l2) ->
          let proof1 = prove (repl_concl seq l1) in
          let proof2 = prove (repl_concl seq l2) in
            if proof1 = NoProof || proof2 = NoProof then NoProof
            else Right (Conj, [proof1; proof2])
        | LabelLookup.Disj (l1, l2) ->
          let seqlist = [(repl_concl seq l1, fun x -> if x=NoProof then NoProof else (Right (Disj_a, [x])));
                         (repl_concl seq l2, fun x -> if x=NoProof then NoProof else (Right (Disj_b, [x])))] in
            prove_disj_list seqlist (fun (seq, fun1) -> fun1 (prove seq))
        | LabelLookup.Imp (l1, l2) ->
          let new_seq = repl_concl seq l2 in 
          let new_seq_l1 = add_to_local new_seq l1 in
          let p1 = 
            if List.mem l1 local_list then (prove new_seq) 
            else (prove (update_seq_bset_imp_r2 new_seq_l1))
          in
            if p1=NoProof then p1 else Right (Imp, [p1])
        | LabelLookup.Box (l1,lu) ->
					if BSetR.mem c boxR then NoProof
          else
            let new_seq = boxr_empty_local seq lastBoxL c in
            let new_seq = repl_concl new_seq l1 in
			  		let new_seq = update_seq_bset_box_r new_seq c in
            let p = prove new_seq in
              if p = NoProof then p else Right (Box, [p])
        | LabelLookup.Dia (l1,lu) ->
          let new_seq = (f,g,l,l1,bset_all) in
          let new_seq_r2 = update_seq_bset_dia_r2 (f,g,l,l1,bset_all) c in
          let seqh = (new_seq, fun x -> if x=NoProof then NoProof else Right (Dia_a,[x])) in
					let seqt = if BSetR.mem c diaR then [] else 
            (List.map (fun x -> 
                        let (_,_,(ctxt_list, ctxt_id),_,_) = x in
                          (x, fun y -> if y=NoProof then NoProof else Right (Dia_b ctxt_id, [y])))
                     (switch_ctxt_all new_seq_r2)) in
          let seqlist = seqh :: seqt in
            prove_disj_list seqlist (fun (seq, fun1) -> fun1 (prove seq))
      in
  
      let build_goal_seq (f,g,(_, local_id),c,bset_all) = (c, local_id) in

      let update_goalset new_goalset seq =
        let (f,g,l,c,bset_all) = seq in
        let this_goal = build_goal_seq seq in
        let new_goalset : BSetG.t = BSetG.remove this_goal new_goalset in
        let boxr, diar, disjr, botr, initr, boxt, diat, boxR, diaR, lastBoxL, goalset, pset = bset_all in
        let bset_all : bookset_all = 
          (boxr, diar, disjr, botr, initr, boxt, diat, boxR,diaR,lastBoxL,
          BSetG.union goalset new_goalset, pset) in
        let ret : sequent = (f,g,l,c,bset_all) in
          ret
      in
  
      let focus_label_local label =
        let rule_list = 
          try Rule.find rule_map label
          with Not_found -> []
        in
        let fun1 (ruleid, rule) =
          let lapp = L1 (ruleid, local_id) in
          if applicable seq rule lapp then begin
            let _ = debug_print ("focus_label_local "^(print seq)^" "^(print_left rule_map lapp)) in
            let (trunks, twigs) = apply seq rule lapp in
            let goalset = List.map build_goal_seq trunks in 
            let goalset = goalset @ (List.map build_goal_seq twigs) in
            let goalset : BSetG.t = List.fold_left (fun s e -> BSetG.add e s) BSetG.empty goalset in
            let trunks : sequent list = List.map (update_goalset goalset) trunks in
            let twigs : sequent list = List.map (update_goalset goalset) twigs in 
            let trunk_proofs = prove_conj_list trunks prove in
            let twig_proofs = prove_conj_list twigs prove in
              if List.mem NoProof trunk_proofs then NoProof
              else if List.mem NoProof twig_proofs then NoProof
              else Left (lapp,trunk_proofs,twig_proofs)
          end else NoProof
        in
          prove_disj_list rule_list fun1
      in 
   
     let focus_label_frame ctxt_id label =
        let rule_list = Rule.find rule_map label in
        let fun1 (ruleid, rule) = 
          let lapp = L3 (ruleid, ctxt_id) in
          if applicable seq rule lapp then begin
            let _ = debug_print ("focus_label_frame : "^(print seq)^" "^(print_left rule_map lapp)) in
            let (trunks, twigs) = apply seq rule lapp in
            let goalset = List.map build_goal_seq trunks in 
            let goalset = goalset @ (List.map build_goal_seq twigs) in
            let goalset = List.fold_left (fun s e -> BSetG.add e s) BSetG.empty goalset in
            let trunks = List.map (update_goalset goalset) trunks in
            let twigs = List.map (update_goalset goalset) twigs in 
            let trunk_proofs = prove_conj_list trunks prove in
            let twig_proofs = prove_conj_list twigs prove in
              if List.mem NoProof trunk_proofs then NoProof
              else if List.mem NoProof twig_proofs then NoProof
              else Left (lapp, trunk_proofs, twig_proofs)
          end else NoProof
        in
          prove_disj_list rule_list fun1
      in
    
      let rec focus_frame_ctxt (ctxt_list, ctxt_id) =
        let fun2 label =
          focus_label_frame ctxt_id label
        in
          prove_disj_list ctxt_list fun2
      in
 
      let prove_local _ =
        let _ = debug_print "> prove_local" in
        prove_disj_list local_list focus_label_local
      in

      let prove_global_L1 _ =
        let _ = debug_print "> prove_global_L1" in
        prove_disj_list global_list focus_label_local
      in

      let prove_global_L3 _ =
        let _ = debug_print "> prove_global_L3" in
        let fun1 (ctxt_list, ctxt_id) =
          prove_disj_list global_list (focus_label_frame ctxt_id)
        in
          prove_disj_list f fun1
      in

      let prove_frame _ =
        let _ = debug_print "> prove_frame" in
        prove_disj_list f focus_frame_ctxt
      in
  
      let run_prove_funs _ =
        let _ = debug_print "> prove_left" in
        let prove_funs = [prove_local; prove_global_L1; prove_global_L3; prove_frame; prove_right] in
          List.fold_left (fun lastp f -> if lastp=NoProof then (f ()) else lastp) NoProof prove_funs 
      in

      let cur_depth = !call_level in
      let mark_hist_added = ref false in
      let seq_digest = seq_to_digest seq in
      let mark_idx_rev = ref 0 in
      let covered_by = 
        if cur_depth > 15 then test_in_history seq else(
        if cur_depth+1 > !max_depth_alive then begin
          debug_print (print seq);
          print_bset_all bset_all; 
          let hist_check = test_in_history seq in
            if hist_check=None then begin
              depth_history:=(seq, NotYet)::!depth_history;
              mark_idx_rev := List.length !depth_history;
              mark_hist_added:=true;
              None end 
            else hist_check 
        end else None)
      in
      let _ = debug_print (print seq) in
      let _ = call_mark () in
      let _ = branch_history:=seq::!branch_history in
      let digest_res = try MapDigest.find seq_digest !map_seq_digest
                       with Not_found -> NotYet in
      let result = 
        if test_parents seq then NoProof 
        else if digest_res!=NotYet then let _ = debug_print "digest found" in digest_res
        (* goalset test if the pair of (local context id, goal formula label) is in the goal set, then it is already done *)
        (* the goalset consists of the all the goal id for those sequents in the same domain with lower level *)
        else if goal_id_redun local_id c goalset then Done  
        else if covered_by=Some NoProof then NoProof
        else if covered_by=Some Done then Done 
        else begin
          (* the seq survives all the tests. the seq is worth trying to prove (alive) *)
          if cur_depth > !max_depth_alive then max_depth_alive := cur_depth;
          (* phc debug, open bracket without indentation *)
          if !debug then print_endline "[";
          debug_print (print seq); 
          print_bset_all bset_all; 
          (* main body of prover which tries to prove the seq by applying left rules and right rules *)
          try run_prove_funs ()
          with MaxDepth -> begin
            print_endline "==============================================";
            print_endline (string_of_int cur_depth);
            print_endline (print seq);
            print_bset_all bset_all;
            raise MaxDepth end
          | _ -> 
            (* does not allow exceptions other than MaxDepth *)
            print_endline "what is this exception?";
            raise MaxDepth
        end
      in
      (* post prover operations *) 
      let _ = call_mark_ret () in
      let _ = branch_history:=List.tl !branch_history in
 	    let _ = if result=NoProof 
              then let _ = map_seq_digest := MapDigest.add seq_digest NoProof !map_seq_digest in
                    debug_print ("Fail : "^(print seq))
              else let _ = map_seq_digest := MapDigest.add seq_digest Done !map_seq_digest in
                   debug_print ("Succeed : "^(print seq)) in
      (* update history list *)
      let rec update_middle l1 l2 idx fn =
        if l2 = [] then let _ = print_endline "update middle error" in [] 
        else if List.length l2 = idx then l1@((fn (List.hd l2))::(List.tl l2))
        else update_middle (l1 @ [(List.hd l2)]) (List.tl l2) idx fn
      in
      
      let update_hist_list _ = update_middle [] !depth_history !mark_idx_rev 
                (function (seqh, NotYet) -> (seqh, if result=NoProof then NoProof else Done) 
                   | _ -> let _ = print_endline "update middle error2" in raise Not_found)
      in

      let _ = if !mark_hist_added then depth_history:=update_hist_list () else () in
      (* notifies when call and ret marks are not consistent *)
      let _ = if !call_level != test_level then 
                let _ = Printf.printf "%d %d" (!call_level) test_level in
                raise LevelError 
              else () in 
      let _ = if !debug then print_endline "]" else () in
        result
    end end in
      prove seq
  
  exception Reconstruct

  let rec print_proof seq p lookup rule_map =
    let (f,g,l,c,bset_all) = seq in
    let boxr, diar, disjr, botr, initr, boxt, diat, boxR, diaR, lastBoxL, _, _ = bset_all in
    let local_list, local_id = l in
    let comp, s = LabelLookup.find lookup c in
    let proof_struct =
      match p with
        Right (Top, _) -> ("\\Top", [])
      | Right (Conj, plist) -> 
          let l1, l2 = 
            match comp with 
              LabelLookup.Conj (l1,l2) -> (l1,l2) 
            | _ -> raise Reconstruct
          in
          let proof1, proof2 =
            match plist with 
              proof1::proof2::[] -> (proof1, proof2)
            | _ -> raise Reconstruct
          in
          let seq1 = (repl_concl seq l1) in
          let seq2 = (repl_concl seq l2) in
            ("\\wedge R", [print_proof seq1 proof1 lookup rule_map;
                           print_proof seq2 proof2 lookup rule_map])
      | Right (Disj_a, plist) ->
          let l1, l2 = 
            match comp with
              LabelLookup.Disj (l1,l2) -> (l1, l2) 
            | _ -> raise Reconstruct
          in
          let proof1 = match plist with p1::[] -> p1 | _ -> raise Reconstruct in
          let seq1 = repl_concl seq l1 in
            ("\\vee R_a", [print_proof seq1 proof1 lookup rule_map])
      | Right (Disj_b, plist) ->
          let l1, l2 =
            match comp with
              LabelLookup.Disj (l1,l2) -> (l1,l2)
            | _ -> raise Reconstruct
          in
          let proof2 = match plist with p2::[] -> p2 | _ -> raise Reconstruct in
          let seq2 = repl_concl seq l2 in
            ("\\vee R_b", [print_proof seq2 proof2 lookup rule_map])
      | Right (Imp, plist) ->
          let l1, l2 =
            match comp with
              LabelLookup.Imp (l1,l2) -> (l1,l2)
            | _ -> raise Reconstruct
          in
          let proof1 = match plist with p1::[] -> p1 | _ -> raise Reconstruct in
          let seq1 = repl_concl seq l2 in
          let seq1 = if List.mem l1 local_list then seq1 
                     else add_to_local seq1 l1 
          in
          let premise = print_proof seq1 proof1 lookup rule_map in
            ("\\supset R", [premise])
      | Right (Box, plist) ->
          let l1 = match comp with LabelLookup.Box (l1,lu) -> l1 
                     | _ -> raise Reconstruct
          in
          let proof1 = match plist with p1::[] -> p1 | _ -> raise Reconstruct in
          let new_seq = boxr_empty_local seq lastBoxL c in
          let new_seq = repl_concl new_seq l1 in
          let premise = print_proof new_seq proof1 lookup rule_map in
            ("\\Box R", [premise])
      | Right (Dia_a, plist) ->
          let l1 = 
            match comp with LabelLookup.Dia (l1,lu) -> l1 | _ -> raise Reconstruct
          in
          let proof1 = match plist with p1::[] -> p1 | _ -> raise Reconstruct in
          let seq1 = repl_concl seq l1 in
          let premise = print_proof seq1 proof1 lookup rule_map in
            ("\\Diamond R_1", [premise])
      | Right (Dia_b ctxt_id, plist) ->
          let l1 = 
            match comp with LabelLookup.Dia (l1,lu) -> l1 | _ -> raise Reconstruct
          in
          let proof1 = match plist with p1::[] -> p1 | _ -> raise Reconstruct in
          let seq1 = switch_ctxt (repl_concl seq l1) ctxt_id in
          let premise = print_proof seq1 proof1 lookup rule_map in
            ("\\Diamond R_2", [premise])
      | Left (L1 (ruleid, ctxt_id), trunk_proof, twig_proof) ->
          let rule = Rule.findi rule_map ruleid in
          let trunk_seq, twig_seq = apply_noset seq rule (L1 (ruleid,ctxt_id)) in
          let print_pair = fun seq proof -> print_proof seq proof lookup rule_map in
          let trunk_str = List.map2 print_pair trunk_seq trunk_proof in
          let twig_str = List.map2 print_pair twig_seq twig_proof in
            (print_left rule_map (L1 (ruleid, ctxt_id)), List.concat [trunk_str;twig_str])
      | Left (L3 (ruleid, ctxt_id), trunk_proof, twig_proof) ->
          let rule = Rule.findi rule_map ruleid in
          let trunk_seq, twig_seq = apply_noset seq rule (L3 (ruleid,ctxt_id)) in
          let print_pair = fun seq proof->print_proof seq proof lookup rule_map in
          let trunk_str = List.map2 print_pair trunk_seq trunk_proof in
          let twig_str = List.map2 print_pair twig_seq twig_proof in
            (print_left rule_map (L3 (ruleid, ctxt_id)), List.concat [trunk_str;twig_str])
      | Done -> ("Done", [])
      | NoProof -> ("NoProof", [])
      | NotYet -> raise (ProverError "print_proof encounters a NotYet node")
    in
    let rule, premises = proof_struct in
    let concl_str = print seq in
    let premise_str = List.fold_left (fun x y->x^" "^y) "" premises in
      "\\infer["^rule^"] {"^concl_str^"}{"^premise_str^"}"
  end;;

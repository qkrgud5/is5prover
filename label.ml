(* This file is distributed under the terms of
   the GNU Lesser General Public License Version 2.1 *)
(* Hyungchul Park, Sungwoo Park *)
(* Programming Language Laboratory, POSTECH *)
(* {luscani, gla}@postech.ac.kr *)


open Formula
open Def

module type LabelSig = sig
  type t
  type map

  val compare : t -> t -> int
  val empty : map
  val add : map -> Formula.t -> map
  val find : map -> Formula.t -> t
  val iter : map -> 'a -> (t -> 'a -> 'a) -> 'a
  val print_map : map -> string
  val print : t -> string
  val print_tex : Formula.t -> map -> string

  val labeling : Formula.t -> map
end;;

module Label : LabelSig = struct
	(* label is a number for each formula. 
		If two formulas are identical, then they are mapped to the same label.
		Label.map type maintains the mapping info *)
  type t = int
  type map = (Formula.t * int) list

  let compare = compare
  let empty = []
  let add m f =
    let rec add1 m f n =
      match m with
        [] -> [(f, n)]
      | (fh, nh)::t -> if fh=f then m else (fh, nh)::(add1 t f (n+1))
    in
      add1 m f 1

  let rec find m f =
    match m with
      [] -> let _ = print_string "Not_found error at Label.find:\n" in
            let _ = print_string (Formula.print f); print_string "\n" in
              let _ = print_string "in Label find" in raise Not_found
    | (fh, nh)::t -> if fh=f then (nh) else (find t f)

  let rec iter m a f =
    match m with
      [] -> a
    | (fh, nh)::t -> iter t (f nh a) f

  let rec print_map m =
    match m with
      [] -> ""
    | (fh, nh)::t -> (string_of_int nh) ^ " : " ^ (Formula.print fh) ^ "\n" ^(print_map t)

  let rec print l =
    string_of_int l

	
  let labeling f =
    let rec fun1 f lmap =
      let new_lmap = add lmap f in
      match f with
        Formula.Top -> new_lmap
      | Formula.Bot -> new_lmap
      | Formula.Atom p -> new_lmap
      | Formula.Conj (f1, f2) ->
          let lmap1 = fun1 f1 new_lmap in
          let lmap2 = fun1 f2 lmap1 in
            lmap2
      | Formula.Disj (f1, f2) ->
          let lmap1 = fun1 f1 new_lmap in
          let lmap2 = fun1 f2 lmap1 in
            lmap2
      | Formula.Imp (f1, f2) ->
          let lmap1 = fun1 f1 new_lmap in
          let lmap2 = fun1 f2 lmap1 in
            lmap2
      | Formula.Box f1 ->
          fun1 f1 new_lmap
      | Formula.Dia f1 ->
          fun1 f1 new_lmap
    in
      fun1 f empty


  type primitive_sign = PL1 | PL2 | PF1 | PF2 | PR

  let print_tex f lmap = 
    let tex_list = ref [] in
    let rec fun1 f prec s lmap =
      let prec_f = Formula.prec f in
      let str = 
        match f with
          Formula.Top -> Def.Tex.top
        | Formula.Bot -> Def.Tex.bot
        | Formula.Atom p -> Formula.print f
        | Formula.Conj (f1, f2) -> 
          let s1 = (if s=PR then PR else PF1) in
					let str1 = fun1 f1 prec_f s1 lmap in
					let str2 = fun1 f2 prec_f s1 lmap in
						Printf.sprintf "%s %s %s" str1 (Def.Tex.conj) str2
        | Formula.Disj (f1, f2) ->
          let s1 = (if s=PR then PR else PL1) in
          let s2 = (if s=PR then PR else PL1) in
					let str1 = fun1 f1 prec_f s1 lmap in
					let str2 = fun1 f2 prec_f s2 lmap in
						Printf.sprintf "%s %s %s" str1 Def.Tex.disj str2
        | Formula.Imp (f1, f2) ->
          let s1 = (if s=PR then PL1 else PR) in
          let s2 =
            match s with
              PR -> PR | PL1 -> PF1 | PF1 -> PF1
            | PL2 -> PF2 | PF2 -> PF2
          in
					let str1 = fun1 f1 prec_f s1 lmap in
          let str2 = fun1 f2 prec_f s2 lmap in
						Printf.sprintf "%s %s %s" str1 Def.Tex.imp str2
        | Formula.Box f1 -> 
          let s1 = (if s=PR then PR else PL2) in
          "\\Box" ^ (fun1 f1 prec_f s1 lmap)
        | Formula.Dia f1 -> 
          let s1 = (if s=PR then PR else PL1) in
          "\\Diamond" ^ (fun1 f1 prec_f s1 lmap)
      in
      let sign_str = 
        match s with
          PL1 -> "-, \\sim"
        | PL2 -> "=, \\approx"
        | PF1 -> "\\sim"
        | PF2 -> "\\approx"
        | PR -> "+"
      in
      let tex_str = 
        "\\overbrace{" ^ str ^ "}^{{L_{" ^
        (print (find lmap f)) ^ "}}^{" ^ sign_str ^ "}}"
      in
        if prec < prec_f then
          ("(" ^ tex_str ^ ")")
        else
          if String.length tex_str > 2000 then
            (let _ = tex_list := tex_str :: (!tex_list) in
              "L_{" ^ (print (find lmap f)) ^ "}")
          else
            tex_str
    in
    let tex_str = fun1 f 10 PR lmap in
    let _ = tex_list := tex_str::!tex_list in
      List.fold_right (fun x y -> y ^ "$$\n" ^ x ^ "\n$$\n\n") (!tex_list) ""

end;;


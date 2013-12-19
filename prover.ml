(* This file is distributed under the terms  
   the GNU Lesser General Public License Version 2.1 *)
(* Hyungchul Park, Sungwoo Park *)
(* Programming Language Laboratory, POSTECH *)
(* {luscani, gla}@postech.ac.kr *)                      

open Formula
open Label
open Labellookup
open Rule
open Sequent

let target_path = ref "result"
let num = ref 0
let debug = ref false

let welcoming _ =
print_endline "-------------------------------------------------------------------";
print_endline "                        Welcome to IS5 prover                      ";
print_endline "-------------------------------------------------------------------";
print_endline "Input Syntax";
print_endline "P := [a-Z] ([alphanum])*            # atomic formulas";
print_endline "A := P | true | false | ~ A | A & A | A v A | A -> A | box A | dia A | (A)";
print_endline "     | A <-> A         # syntatic sugar (A<->B means (A->B & B->A))";
print_endline "";
print_string  "Default output tex file is set to ";
print_endline (!target_path);
print_endline "You may input a formula to prove it or 'q' to exit.";;

let no_proof = ref false
let script = ref ""
let input_path = ref ""

let usage = "usage: " ^ Sys.argv.(0) ^ " [-i input_file] [-o output_tex] [-n] [-s shell_script]"

let speclist = [
  ("-i", Arg.String (fun s -> input_path := s), ": set optional input file path, the input file should contain lines of formulas");
  ("-o", Arg.String (fun s -> target_path := s), ": set optional file path for tex file");
  ("-n", Arg.Set no_proof, ": this switch prevent proof search");
  ("-s", Arg.String (fun s -> script := s), ": shell scripts which runs after generating tex output file");
  ("-d", Arg.Set debug, ": debug mode")]

let () = 
  Arg.parse speclist (fun x -> raise (Arg.Bad ("Bad arguments : " ^ x))) usage;;

let quit_loop = ref false;;

let prove str = 
  let f = try Formula.parse str 
          with Parsing.Parse_error -> raise (Failure "Parsing error. See the input syntax.") in
  let lmap = Label.labeling f in
  let l = Label.find lmap f in
  let lookup = LabelLookup.build f lmap in
  (* let _ = print_endline (LabelLookup.print_map lmap lookup) in*)
  let rule_map = Rule.rule_gen lmap lookup in
  let seq = Sequent.create l in

  let formula_tex = "\n" ^ (Label.print_tex f lmap) ^ "\n\n" in
  let rules_tex = Rule.print_map lmap rule_map in
  let proof_tex = 
    if !no_proof then "" else
    let t = Sys.time() in 
    try 
      ("$$\n" ^ (
        let proof = Sequent.prove_fun seq lookup rule_map in
        let _ = if proof = Sequent.NoProof then raise Not_found else () in 
        let _ = Printf.printf "proof found.\n\ntime to prove : %fs\n# of prove function call : %d\n\n" 
                (Sys.time() -. t) (Sequent.get_call_cnt ()) in
        let _ = Printf.printf "search space (# of seqs) : %d\n" (Sequent.get_search_space()) in
        Sequent.print_proof seq proof lookup rule_map
       ) ^ "\n$$\n\n")
    with Not_found -> 
      let _ = Printf.printf "proof not found.\n\ntime to prove : %fs\n" (Sys.time() -. t) in
      let _ = Printf.printf "search space (# of seqs) : %d\n" (Sequent.get_search_space()) in
        "proof not found"
  in
  let _target_path = !target_path ^ (Printf.sprintf "%03d" !num) ^ ".tex" in
  let _ = Printf.printf "Input the result tex file name.\nOr press enter to output to default file %s > " (_target_path) in
  let _ = num := !num + 1 in
  let outpath = read_line () in
  let outpath = if outpath="" then _target_path else outpath in
  let outfile = open_out outpath in
  let _ = output_string outfile (Latex.template formula_tex rules_tex proof_tex) in
  let _ = close_out outfile in
    ();;

let trim_last_slash str =
  let last = if str="" then "" else (String.sub str (String.length str - 1) 1) in
  let trunc_last str = if str="" then "" else (String.sub str 0 (String.length str - 1)) in
    if last = "\\" then (trunc_last str)
    else str;;

let is_last_slash str = 
  let last = if str="" then "" else (String.sub str (String.length str - 1) 1) in
    last = "\\";;


let input_from_file _ = 
  let infile = open_in (!input_path) in
  let quit_loop = ref false in
    while not !quit_loop do
      let str = try input_line infile with End_of_file -> quit_loop:=true; "q" in
      print_endline "";
      print_endline "------------------------------------------------";
      print_endline str;
      if str="q" || str="quit" then 
        quit_loop:=true
      else (if !quit_loop then () else (
        try 
          let _ = prove str in
          let _ = if !script="" then 0 else (Sys.command (!script)) in
            ()
        with Failure str -> print_endline str
      ))
    done;;

let input_from_typing _ =
  welcoming ();
  while not !quit_loop do
    print_string "> ";
    let str = ref "" in
    let cont_input = ref true in
      while !cont_input do
        let input = read_line () in
          str := !str ^ (trim_last_slash input);
          cont_input := is_last_slash input 
      done;
    let str = !str in
    if str="q" || str="quit" then
      quit_loop := true
    else
      try
        let _ =  prove str in
        let _ = if !script="" then 0 else (Sys.command (!script)) in
          ()
      with Failure str -> print_endline str
  done;;


if !debug then Sequent.set_debug();
if !input_path="" then input_from_typing () else input_from_file ()

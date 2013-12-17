(* This file is distributed under the terms of
   the GNU Lesser General Public License Version 2.1 *)
(* Hyungchul Park, Sungwoo Park *)
(* Programming Language Laboratory, POSTECH *)
(* {luscani, gla}@postech.ac.kr *)

(* lexer tokens *)
{
open Parser
open Def
exception Eof
}

rule token = parse
  [' ' '\t']	{token lexbuf}
| ['\n']	{EOL}
| "true" {TOP}
| "false" {BOT}
| '~' {NEG}
| "&" {CONJ}
| "v" {DISJ}
| "->" {IMP}
| "<->" {LEQ}
| "box" {BOX}
| "dia" {DIA}
| '(' {LPAREN}
| ')' {RPAREN}
| ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm {ATOM(lxm)}
| eof		{raise Eof}


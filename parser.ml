type token =
  | ATOM of (string)
  | TOP
  | BOT
  | CONJ
  | DISJ
  | IMP
  | LEQ
  | BOX
  | DIA
  | NEG
  | LPAREN
  | RPAREN
  | EOL

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"
# 20 "parser.ml"
let yytransl_const = [|
  258 (* TOP *);
  259 (* BOT *);
  260 (* CONJ *);
  261 (* DISJ *);
  262 (* IMP *);
  263 (* LEQ *);
  264 (* BOX *);
  265 (* DIA *);
  266 (* NEG *);
  267 (* LPAREN *);
  268 (* RPAREN *);
  269 (* EOL *);
    0|]

let yytransl_block = [|
  257 (* ATOM *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\003\000\003\000\003\000\003\000\
\003\000\002\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\003\000\004\000\000\000\000\000\000\000\
\000\000\013\000\000\000\010\000\011\000\012\000\000\000\000\000\
\000\000\000\000\000\000\001\000\005\000\006\000\000\000\000\000\
\000\000"

let yydgoto = "\002\000\
\010\000\011\000"

let yysindex = "\005\000\
\006\255\000\000\000\000\000\000\000\000\006\255\006\255\006\255\
\006\255\000\000\014\255\000\000\000\000\000\000\018\255\006\255\
\006\255\006\255\006\255\000\000\000\000\000\000\025\255\036\255\
\040\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\026\255\248\254\
\022\255"

let yygindex = "\000\000\
\000\000\250\255"

let yytablesize = 47
let yytable = "\012\000\
\013\000\014\000\015\000\008\000\008\000\001\000\003\000\004\000\
\005\000\022\000\023\000\024\000\025\000\006\000\007\000\008\000\
\009\000\016\000\017\000\018\000\019\000\016\000\017\000\018\000\
\019\000\000\000\020\000\009\000\016\000\021\000\007\000\007\000\
\007\000\009\000\009\000\000\000\000\000\007\000\007\000\016\000\
\017\000\018\000\019\000\016\000\017\000\000\000\019\000"

let yycheck = "\006\000\
\007\000\008\000\009\000\012\001\013\001\001\000\001\001\002\001\
\003\001\016\000\017\000\018\000\019\000\008\001\009\001\010\001\
\011\001\004\001\005\001\006\001\007\001\004\001\005\001\006\001\
\007\001\255\255\013\001\006\001\004\001\012\001\005\001\006\001\
\007\001\012\001\013\001\255\255\255\255\012\001\013\001\004\001\
\005\001\006\001\007\001\004\001\005\001\255\255\007\001"

let yynames_const = "\
  TOP\000\
  BOT\000\
  CONJ\000\
  DISJ\000\
  IMP\000\
  LEQ\000\
  BOX\000\
  DIA\000\
  NEG\000\
  LPAREN\000\
  RPAREN\000\
  EOL\000\
  "

let yynames_block = "\
  ATOM\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'formula) in
    Obj.repr(
# 21 "parser.mly"
                (_1)
# 115 "parser.ml"
               : Def.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 24 "parser.mly"
          (Def.Atom _1)
# 122 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    Obj.repr(
# 25 "parser.mly"
         (Def.Top)
# 128 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    Obj.repr(
# 26 "parser.mly"
         (Def.Bot)
# 134 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'formula) in
    Obj.repr(
# 27 "parser.mly"
                         (_2)
# 141 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 28 "parser.mly"
                        (Def.Conj (_1,_3))
# 149 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 29 "parser.mly"
                        (Def.Disj (_1,_3))
# 157 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 30 "parser.mly"
                        (Def.Imp (_1,_3))
# 165 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 31 "parser.mly"
                        (Def.Conj (Def.Imp (_1,_3), Def.Imp (_3,_1)))
# 173 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 32 "parser.mly"
                 (Def.Box _2)
# 180 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 33 "parser.mly"
                 (Def.Dia _2)
# 187 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 34 "parser.mly"
                    (Def.Neg _2)
# 194 "parser.ml"
               : 'formula))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Def.t)

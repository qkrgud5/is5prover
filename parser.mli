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

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Def.t

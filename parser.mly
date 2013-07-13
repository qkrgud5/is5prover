
%{
%}
%token <string> ATOM
%token TOP BOT
%token CONJ DISJ IMP LEQ
%token BOX DIA NEG
%token LPAREN RPAREN
%token EOL
%right IMP
%right LEQ 
%left DISJ
%left CONJ 
%nonassoc NEG
%nonassoc BOX
%nonassoc DIA
%start main
%type <Def.t> main
%%
main:
  formula EOL			{$1}
;
formula:
  ATOM				{Def.Atom $1}
| TOP				{Def.Top}
| BOT				{Def.Bot}
| LPAREN formula RPAREN		{$2}
| formula CONJ formula		{Def.Conj ($1,$3)}
| formula DISJ formula		{Def.Disj ($1,$3)}
| formula IMP  formula		{Def.Imp ($1,$3)}
| formula LEQ  formula		{Def.Conj (Def.Imp ($1,$3), Def.Imp ($3,$1))}
| BOX formula 			{Def.Box $2}
| DIA formula 			{Def.Dia $2}
| NEG formula       {Def.Neg $2}
;

%token <Num.num> Int
%token <float> Float
%token <string> Atom
%token <string> String
%token <token list> E_tuple E_list
%token L_bracket R_bracket
%token L_stache R_stache
%token Comma Stop
%start main
%type <token> main
%%
main:
    expr Stop                    { $1 }
;
expr:
  | Int                          { Int $1 }
  | Float                        { Float $1 }
  | String                       { String $1 }
  | Atom                         { Atom $1 }
  | L_stache sequence R_stache   { E_tuple (List.rev $2) }
  | L_bracket sequence R_bracket { E_list (List.rev $2) }
;
sequence:
  | /* empty */                  { [] }
  | sequence1                    { $1 }
;
sequence1:
  | expr                         { [$1] }
  | sequence1 Comma expr         { $3::$1 }
;

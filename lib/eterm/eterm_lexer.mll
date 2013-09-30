{
  open Eterm_parser
}
rule token = parse
  | [' ' '\t' '\n']                                   { token lexbuf }
  | ['0'-'9']+ as num                                 { Int (Num.num_of_string num) }
  | ['0'-'9']+'.'['0'-'9']+ as num                    { Float (float_of_string num) }
  | ['a'-'z'][^'\'' '[' ']' '{' '}' '.' ' ']* as atom { Atom atom }
  | '\''[^'\'']*'\'' as atom                          { Atom atom }
  | '"'[^'"']*'"' as string                           { String string }
  | '.'                                               { Stop }
  | ','                                               { Comma }
  | '['                                               { L_bracket }
  | ']'                                               { R_bracket }
  | '{'                                               { L_stache }
  | '}'                                               { R_stache }

(* Ocamllex scanner for OOPs lang*)

{ 
open Parser 

let strip_quotes str =
  match String.length str with
  | 0 | 1 | 2 -> ""
  | len -> String.sub str 1 (len - 2)
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let stringliteral = '"'('\\''.'|[^'\\''"'])*'"'

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*" { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '.'      { DOT }
| '}'      { RBRACE }
| '['      { LBRACK }
| ']'      { RBRACK }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '%'      { MOD }
| '='      { ASSIGN }
| "&&"     { AND }
| "||"     { OR }
| "lt"     { LESSER }
| "lte"    { LESSEREQ}
| "gt"     { GREATER }
| "gte"    { GREATEREQ}
| "eq"     { EQ }
| "ne"     { NEQ }
| "if"     { IF }
| "else"   { ELSE }
| "while"  { WHILE }
| "for"    { FOR }
| "return" { RETURN }
| "int"    { INT }
| "bool"   { BOOL }
| "str"    { STR }
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| "private" { PRIVATE }
| "public" { PUBLIC }
| "protected" { PROTECTED }
| "class"   { CLASS }
| "new" { NEW }
| ['A'-'Z'] letter* as lem { OBJECT(lem) }
| digit+ as lem  { NUM(int_of_string lem) }
| stringliteral as lem { STRLIT(strip_quotes lem) }
| letter (digit | letter | '_')* as lem { ID(lem) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
  | _     { comment lexbuf }


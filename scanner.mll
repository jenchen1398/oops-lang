(* Ocamllex scanner for OOPs lang*)

(*{ open Microcparse }*)

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "#"     { COMMENT }           (* Comments *)
| '_'     { UNDERSCORE }
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRACK }
| ']'      { RBRACK }
| '"'      { QUOTE }
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
| ">"      { RREDIR }
| "<"      { LREDIR }
| ">>"     { APPENDREDIR }
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
| "assert"   { ASSERT }
| digit+ as lem  { NUM(int_of_string lem) }
| letter (digit | letter | '_')* as lem { ID(lem) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }


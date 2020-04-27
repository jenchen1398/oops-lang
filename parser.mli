type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MOD
  | ASSIGN
  | EQ
  | NEQ
  | LESSER
  | LESSEREQ
  | GREATER
  | GREATEREQ
  | AND
  | OR
  | IF
  | ELSE
  | WHILE
  | FOR
  | INT
  | BOOL
  | STR
  | RETURN
  | COMMA
  | DOT
  | CLASS
  | PRIVATE
  | PUBLIC
  | PROTECTED
  | NEW
  | CONS
  | NUM of (int)
  | BLIT of (bool)
  | STRLIT of (string)
  | ID of (string)
  | OBJECT of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program

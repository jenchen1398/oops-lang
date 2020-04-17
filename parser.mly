/* Ocamlyacc parser for MicroC */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK PLUS MINUS TIMES DIVIDE MOD ASSIGN
%token EQ NEQ LESSER LESSEREQ GREATER GREATEREQ AND OR
%token IF ELSE WHILE FOR INT BOOL STR
/* return, COMMA token */
%token RETURN COMMA 
%token CLASS PRIVATE PUBLIC PROTECTED
%token <int> NUM
%token <bool> BLIT
%token <string> STRLIT
%token <string> ID
%token EOF

%start program
%type <Ast.program> program

%left 
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LESSER LESSEREQ GREATER GREATEREQ 
%left PLUS MINUS
%left TIMES DIVIDE
%left MOD

%%

/* add function declarations*/
program:
  class_decls decls EOF { ($1, fst $2, snd $2) }

class_decls:
  /*nothing*/ { [] }
  | cdecl class_decls { $1 :: $2 }

cdecl:
  modifer CLASS ID LBRACE decls RBRACE
  {
    {
      cmod = $1;
      cname = $3;
      vars = fst $5;
      funcs = snd $5
    }
  }

modifer:
   PRIVATE { Private }
 | PUBLIC { Public }
 | PROTECTED { Protected }

decls:
   /* nothing */ { ([], [])              }
 | vdecl SEMI decls { (($1 :: fst $3), snd $3) }
 | fdecl decls { (fst $2, ($1 :: snd $2)) }

vdecl_list:
  /*nothing*/ { [] }
  | vdecl SEMI vdecl_list  {  $1 :: $3 }

/* int x */
vdecl:
    typ ID { ($1, $2) }

typ:
  primitive { $1 }

primitive:
    INT   { Int   }
  | BOOL  { Bool  }
  | STR   { String }

/* fdecl */
fdecl:
  vdecl LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
  {
    {
      rtyp=fst $1;
      fname=snd $1;
      formals=$3;
      locals=$6;
      body=$7
    }
  }

/* formals_opt */
formals_opt:
  /*nothing*/ { [] }
  | formals_list { $1 }

formals_list:
  vdecl { [$1] }
  | vdecl COMMA formals_list { $1::$3 }

stmt_list:
  /* nothing */ { [] }
  | stmt stmt_list  { $1::$2 }

stmt:
    expr SEMI                               { Expr $1      }
  | LBRACE stmt_list RBRACE                 { Block $2 }
  /* if (condition) { block1} else {block2} */
  /* if (condition) stmt else stmt */
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | WHILE LPAREN expr RPAREN stmt           { While ($3, $5)  }
  | FOR LPAREN expr SEMI expr SEMI expr RPAREN stmt  { For ($3, $5, $7, $9) }
  /* return */
  | RETURN expr SEMI                        { Return $2      }

expr:
    NUM          { Num($1)            }
  | BLIT             { BoolLit($1)            }
  | STRLIT           { StrLit($1)            }
  | ID               { Id($1)                 }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr TIMES   expr { Binop($1, Times,   $3)   }
  | expr DIVIDE   expr { Binop($1, Divide,   $3)   }
  | expr MOD    expr { Binop($1, Mod, $3)     }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq, $3)     }
  | expr LESSER     expr { Binop($1, Lesser,  $3)   }
  | expr LESSEREQ     expr { Binop($1, LesserEq,  $3)   }
  | expr GREATER     expr { Binop($1, Greater,  $3)   }
  | expr GREATEREQ     expr { Binop($1, GreaterEq,  $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | ID ASSIGN expr   { Assign($1, $3)         }
  | LPAREN expr RPAREN { $2                   }
  /* call */
  | ID LPAREN args_opt RPAREN { Call ($1, $3)  }


/* args_opt*/
args_opt:
  /*nothing*/ { [] }
  | args { $1 }

args:
  expr  { [$1] }
  | expr COMMA args { $1::$3 }

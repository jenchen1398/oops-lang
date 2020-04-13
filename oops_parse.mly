/* Ocamlyacc parser for OOPs lang */

%{
open Ast
%}

%token COMMENT 
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token QUOTE SEMI COMMA 
%token PLUS MINUS TIMES DIVIDE MOD
%token LESSER LESSEREQ GREATER GREATEREQ EQ NEQ
%token ASSIGN AND OR
%token RREDIR LREDIR APPENDREDIR  PIPE
%token IF ELSE WHILE FOR RETURN
%token INT BOOL STR 
%token PRIVATE PUBLIC PROTECTED CLASS ASSERT
%token <bool> BLIT
%token <int> NUM
%token <string> ID
%token EOF

%start program
%type <Ast.program> program

%right PIPE
%right RREDIR APPENDREDIR
%left LREDIR
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LESSER LESSEREQ GREATER GREATEREQ EQ NEQ
%left PLUS MINUS 
%left TIMES DIVIDE 
%left MOD

%%

program:
  decls EOF {$1}

decls:
   /* nothing */ { ([], [])               }
 | vdecl SEMI decls { (($1 :: fst $3), snd $3) }
 | fdecl decls { (fst $2, ($1 :: snd $2)) }

vdecl_list:
  /*nothing*/ { [] }
  | vdecl SEMI vdecl_list  {  $1 :: $3 }

/* int x */
vdecl:
  typ ID { ($1, $2) }
/* TODO see line 118 */
/*| typ ID ASSIGN expr { ($1, $2, $4) }  */

typ:
    INT   { Int   }
  | BOOL  { Bool  }
  | STR   { Str   }
  
cform:
  ctyp ID { ($1, $2) }
  
ctyp:
    PUBLIC     { public }
  | PRIVATE    { private }
  | PROTECTED  { }

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

/* cdecl */
cdecl:
  cform LBRACE vdecl_list stmt_list RBRACE
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

comment:
  COMMENT { Comment }

typ:
 INT {Int}
| BOOL {Bool}
| STR {Str}

stmt:
    expr SEMI                               { Expr $1      }   
  | arr  SEMI                               { Arr $1   }
  | LBRACE stmt_list RBRACE                 { Block $2 }
  /* if (condition) { block1} else {block2} */
  /* if (condition) stmt else stmt */
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | WHILE LPAREN expr RPAREN stmt           { While ($3, $5)  }
  /*for (start; end; increment) {block1} */
  | FOR LPAREN expr SEMI expr SEMI expr  RPAREN stmt             { For ($3, $5, $7, $9) } 
  /* return */
  | RETURN expr SEMI                        { Return $2      }  

stmt_list:
  /* nothing */ { [] }
  | stmt stmt_list  { $1::$2 }

expr:
    NUM              { Num($1)            }
  | BLIT             { BoolLit($1)            }
  | ID               { Id($1)                 }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr TIMES  expr { Binop($1, Times, $3)   }
  | expr DIVIDE expr { Binop($1, Divide, $3)  }
  | expr MOD    expr { Binop($1, Mod, $3)     }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq, $3)     }
  | expr LESSER     expr { Binop($1, Lesser,  $3)   }
  | expr LESSEREQ     expr { Binop($1, LesserEq,  $3)   }
  | expr GREATER     expr { Binop($1, Greater,  $3)   }
  | expr GREATEREQ     expr { Binop($1, GreaterEq,  $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | ID ASSIGN expr   { Assign($1, $3)         } /* TODO add option for: int my_id = expr; */
  | LPAREN expr RPAREN { $2                   }
  /* call */
  | ID LPAREN args_opt RPAREN { Call ($1, $3)  }

/* arrays */
/* int[10] my_array; */
/* str[] my_array = ["a", "b", "c", "d"]; */
/* my_array = [1, 2, 3, 4];*/
/* TODO */
arr:
  | typ LBRACK NUM RBRACK ID    { ArrInit($1, $3, $5) } 
  | typ LBRACK RBRACK ID ASSIGN llist { ArrDecl($1, $4, $6) }
  | ID ASSIGN llist              { ArrAssign($1, $2) }

elem: 
  NUM { $1 }
| BLIT { $1 }
| ID  { $1 }

elem_list:
 elem { [$1] }
 | elem COMMA elem_list { $1::$3 }

elements:
/*nothing*/ { [] }
| elem_list   { $1 }

llist:
 LBRACK elements RBRACK { List($1)}

/* Pipes and redirection*/
/*RREDIR LREDIR APPENDREDIR  PIPE */
file:
 ID 								{ File($1)	}

command:
 ID 								{ Command($1, []) }
| ID elem_list 						{ Command($1, $2) }

redirect_pipe:
 command PIPE file					{ Pipe($1, $3) }
| command PIPE redirect_pipe			{ Pipe($1, $3) }

redirect:
 command LREDIR file				{ Redirect($1, Input, $3)  }
| command RREDIR file				{ Redirect($1, Output, $3) }
| command APPENDREDIR file			{ Redirect($1, Append, $3) }

/* PRIVATE PUBLIC PROTECTED CLASS ASSERT */
/* TODO */


/* Ocamlyacc parser for OOPs lang */

%{
open Ast
%}

%token COMMENT UNDERSCORE 
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

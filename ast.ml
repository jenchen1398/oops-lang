(* Abstract Syntax Tree *)

type op = Add | Sub | Times | Divide | Mod | Equal | Neq | Lesser | Greater | GreaterEq | And | Or 

type typ = Int | Bool | Str 

type expr = 
	  Num of int
	| BoolLit of bool
	| Id of string
	| Binop of expr * op * expr
	| Assign of string * expr
	| Call of string * expr list

type stmt =
	  Expr of expr
	| Arr of arr
        | Redirect of redirect
	| Block of stmt list
	| If of expr * stmt * stmt
	| While of expr * stmt
	| For of expr * expr * expr * stmt
	| Return of expr

type arr = 
 	 ArrInit of typ * int * string
       | ArrAssign of string * list
       | ArrDecl of typ * string * list	

type command = 
      Command of string * list

type redirect = 
         InRedirect of command * string
       | OutRedirect of command * string
       | AppendRedirect of command * string

type bind = typ * string

type fdecl = {
	rtyp: typ;
	fname: string;
	formals: bind list;
	locals: bind list;
	body: stmt list;
}

type program = bind list * fdecl list

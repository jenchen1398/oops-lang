(* Abstract Syntax Tree *)

type op = Add | Sub | Times | Divide | Mod | Equal | Neq | Lesser | LesserEq | Greater | GreaterEq | And | Or 
type typ = Int | Bool | String

type expr = 
	  Num of int
	| BoolLit of bool
	| StrLit of string
	| Id of string
	| Binop of expr * op * expr
	| Assign of string * expr
	| Call of string * expr list

type stmt =
	  Expr of expr
	| Block of stmt list
	| If of expr * stmt * stmt
	| While of expr * stmt
	| For of expr * expr * expr * stmt
	| Return of expr

type bind = typ * string

type fdecl = {
	rtyp: typ;
	fname: string;
	formals: bind list;
	locals: bind list;
	body: stmt list;
}

type cdecl = {
	cmod: string;
	cname: string;
	vars: bind list;
	funcs: fdecl list;
}

type program = cdecl list * bind list * fdecl list

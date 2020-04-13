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
	| If of expr * stmt * stmt
	| While of expr * stmt
	| For of expr * expr * expr * stmt
	| Return of expr


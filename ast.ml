(* Abstract Syntax Tree *)

type op = Add | Sub | Times | Divide | Mod | Equal | Neq | Lesser | LesserEq | Greater | GreaterEq | And | Or 
type typ = Int | Bool | String | Obj of string | Array of typ * int
type modifer = Private | Public | Protected

type expr = 
	  Num of int
	| BoolLit of bool
	| StrLit of string
	| Id of string
	| ArrayLit of expr list
	| Binop of expr * op * expr
	| Assign of string * expr
	| ArrayCall of string * int
	| Call of string * expr list
	| MethodCall of string * string * expr list
	| Constructor of string * expr list

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
	cmod: modifer;
	cname: string;
	vars: bind list;
	funcs: fdecl list;
}

type program = cdecl list * bind list * fdecl list

let string_of_op = function 
	  Add -> "+"
	| Sub -> "-"
	| Times -> "*"
	| Divide -> "/"
	| Mod -> "%"
	| Equal -> "eq"
	| Neq -> "ne"
	| Lesser -> "lt"
	| LesserEq -> "lte"
	| Greater -> "gt"
	| GreaterEq -> "gte"
	| And -> "&&"
	| Or -> "||"

let rec string_of_expr = function
	Num(n) -> string_of_int n
	| BoolLit(true) -> "true"
	| BoolLit(false) -> "false"
	| StrLit(s) -> "\"" ^ s ^ "\""
	| Id(s) -> s 
	| ArrayLit(li) -> "[" ^ String.concat ", " (List.map string_of_expr li) ^ "]"
	| Binop(e1, o, e2) ->
		string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
	| Assign(v, e) -> v ^ " = " ^ string_of_expr e
	| ArrayCall(v, n) -> v ^ "[" ^ string_of_int n ^ "]"
	| Call(f, e1) ->
		f ^ "(" ^ String.concat ", " (List.map string_of_expr e1) ^ ")"
	| MethodCall(obj, func, args) ->
		obj ^ "." ^ func ^ "(" ^ String.concat ", " (List.map string_of_expr args) ^ ")"
	| Constructor(obj, args) ->
		obj ^ "(" ^ String.concat ", " (List.map string_of_expr args) ^ ")"

let rec string_of_stmt = function
	  Block(stmts) ->
		  "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
	| Expr(expr) -> string_of_expr expr ^ ";\n"
	| If(e, s1, s2) -> "if (" ^ string_of_expr e ^ ")\n" ^
												string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
	| While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
	| For(e1, e2, e3, s) -> "for (" ^ string_of_expr e1 ^ "; " ^ string_of_expr e2 ^ "; " ^
														string_of_expr e3 ^ ")" ^ string_of_stmt s 
	| Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"

let rec string_of_typ = function
	  Int -> "int"
	| Bool -> "bool"
	| String -> "String"
	| Obj(o) -> o
	| Array(t, n) -> string_of_typ t ^ "[" ^ string_of_int n ^ "]"

let string_of_modifer = function
		Private -> "private"
	| Public -> "public"
	| Protected -> "protected"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl = 
		string_of_typ fdecl.rtyp ^ " " ^
		fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
		")\n{\n" ^
		String.concat "" (List.map string_of_vdecl fdecl.locals) ^
		String.concat "" (List.map string_of_stmt fdecl.body) ^
		"}\n"

let string_of_cdecl cdecl =
	string_of_modifer cdecl.cmod ^ " class " ^ cdecl.cname ^ " {\n" ^
	String.concat "" (List.map string_of_vdecl cdecl.vars) ^
	String.concat "\n" (List.map string_of_fdecl cdecl.funcs) ^
	"}\n"

let string_of_program (classes, vars, funcs) = 
	"\n\nParsed program: \n\n" ^
	String.concat "" (List.map string_of_cdecl classes) ^ "\n" ^
	String.concat "\n" (List.map string_of_vdecl vars) ^ "\n" ^
	String.concat "\n" (List.map string_of_fdecl funcs)
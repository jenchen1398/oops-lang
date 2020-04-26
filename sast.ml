(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx
and sx =
    SNum of int
  | SBoolLit of bool
  | SArrayLit of sexpr list
  | SStrLit of string
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SAssign of string * sexpr
  | SArrayCall of string * int  
  (* call *)
  | SCall of string * sexpr list
  | SArrayCall of string * int
  | SMethodCall of string * string * sexpr list
  | SConstructor of string * sexpr list

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SWhile of sexpr * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  (* return *)
  | SReturn of sexpr

(* func_def: ret_typ fname formals locals body *)
type sfdecl = {
  srtyp: typ;
  sfname: string;
  sformals: bind list;
  slocals: bind list;
  sbody: sstmt list;
}

type scdecl = {
	scmod: modifier;
	scname: string;
	svars: bind list;
	sfuncs: sfdecl list;
}

type sporgram = scdecl list * bind list * sfdecl list


(* Pretty-printing functions *)
let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
        SNum(l) -> string_of_int l
      | SBoolLit(true) -> "true"
      | SBoolLit(false) -> "false"
      | SStrLit(str) -> str
      | SId(s) -> s
      | SArrayLit(li) -> "[" ^ String.concat ", " (List.map string_of_sexpr li) ^ "]"
      | SBinop(e1, o, e2) ->
        string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
      | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
      | SArrayCall(v, n) -> v ^ "[" ^ string_of_int n ^ "]"
      | SCall(f, el) ->
          f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
      | SMethodCall(obj, func, args) ->
          obj ^ "." ^ func ^ "(" ^ String.concat ", " (List.map string_of_sexpr args) ^ ")"
      | SConstructor(obj, args) ->
          obj ^ "(" ^ String.concat ", " (List.map string_of_sexpr args) ^ ")"
  ) ^ ")"

let rec string_of_sstmt = function
    SBlock(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n"
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n"
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
                       string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
  | SFor(e1, e2, e3, s) -> "for (" ^ string_of_sexpr e1 ^ "; " ^ string_of_sexpr e2 ^ "; " ^
              string_of_sexpr e3 ^ ")" ^ string_of_sstmt s 

let string_of_sfdecl fdecl = 
  string_of_typ fdecl.srtyp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.slocals) ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

let string_of_scdecl cdecl =
	string_of_modifier cdecl.scmod ^ " class " ^ cdecl.scname ^ " {\n" ^
	String.concat "" (List.map string_of_vdecl cdecl.svars) ^
	String.concat "\n" (List.map string_of_sfdecl cdecl.sfuncs) ^
	"}\n"

let string_of_sprogram (classes, vars, funcs) = 
	"\n\nParsed program: \n\n" ^
	String.concat "" (List.map string_of_scdecl classes) ^ "\n" ^
	String.concat "\n" (List.map string_of_vdecl vars) ^ "\n" ^
	String.concat "\n" (List.map string_of_sfdecl funcs)

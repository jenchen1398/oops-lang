(* Semantic checking for the OOPs-lang compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (classes, globals, functions) =

  (* Verify a list of bindings has no duplicate names *)
  let check_binds (kind : string) (binds : (typ * string) list) =
    let rec dups = function
        [] -> ()
      |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
        raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in

  (* Make sure no globals duplicate *)
  check_binds "global" globals;
  
  let add_class map cl =
    let dup_err = "duplicate class " ^ cl.cname
    and make_err er = raise (Failure er)
    and cname = cl.cname (* Name of the class *)
    in match cl with (* No duplicate classes *)
      _ when StringMap.mem cname map -> make_err dup_err
    | _ ->  StringMap.add cname cl map
  in
  let class_decls = List.fold_left add_class StringMap.empty classes
  in
  let find_class cname =
    try StringMap.find cname class_decls
    with Not_found -> raise (Failure ("unrecognized class " ^ cname))
  in 

  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls =
    StringMap.add "print" {
      rtyp = Int;
      fname = "print";
      formals = [(Int, "x")];
      locals = []; body = [] } StringMap.empty
  in

  (* Add function name to symbol table *)
  let add_func map fd =
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
      _ when StringMap.mem n built_in_decls -> make_err built_in_err
    | _ when StringMap.mem n map -> make_err dup_err
    | _ ->  StringMap.add n fd map
  in

  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in

  (* Return a function from our symbol table *)
  let find_func s =
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = find_func "main" in (* Ensure "main" is defined *)

  let check_func func =
    (* Make sure no formals or locals are void or duplicates *)
    check_binds "formal" func.formals;
    check_binds "local" func.locals;

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
      if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in

    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
        StringMap.empty (globals @ func.formals @ func.locals )
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec check_expr = function
        Num l -> (Int, SNum l)
      | BoolLit l -> (Bool, SBoolLit l)
      | StrLit str -> (String, SStrLit str)
      | Id var -> (type_of_identifier var, SId var)
      | Assign(var, e) as ex ->
        let lt = type_of_identifier var
        and (rt, e') = check_expr e in
        let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                  string_of_typ rt ^ " in " ^ string_of_expr ex
        in
        (check_assign lt rt err, SAssign(var, (rt, e')))

      | Binop(e1, op, e2) as e ->
        let (t1, e1') = check_expr e1
        and (t2, e2') = check_expr e2 in
        let err = "illegal binary operator " ^
                  string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                  string_of_typ t2 ^ " in " ^ string_of_expr e
        in
        (* All binary operators require operands of the same type*)
        if t1 = t2 then
          (* Determine expression type based on operator and operand types *)
          let t = match op with
              Add | Sub | Times | Divide | Mod when t1 = Int -> Int
            | Equal | Neq -> Bool
            | Lesser | LesserEq | Greater | GreaterEq when t1 = Int -> Bool
            | And | Or when t1 = Bool -> Bool
            | _ -> raise (Failure err)
          in
          (t, SBinop((t1, e1'), op, (t2, e2')))
        else raise (Failure err)
      | Call(fname, args) as call ->
        let fd = find_func fname in
        let param_length = List.length fd.formals in
        if List.length args != param_length then
          raise (Failure ("expecting " ^ string_of_int param_length ^
                          " arguments in " ^ string_of_expr call))
        else let check_call (ft, _) e =
               let (et, e') = check_expr e in
               let err = "illegal argument found " ^ string_of_typ et ^
                         " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
               in (check_assign ft et err, e')
          in
          let args' = List.map2 check_call fd.formals args
          in (fd.rtyp, SCall(fname, args'))
      | ArrayCall(v, n) -> (type_of_identifier v, SArrayCall(v, n))
      | ArrayLit(li) -> let check_array (al : expr list) =
                            let rec dups = function
                                [] -> ()
                              |	(a1 :: a2 :: _) when fst (check_expr a1) != fst (check_expr a2)  ->
                                raise (Failure ("mismatched types of " ^ string_of_typ (fst (check_expr a1) ) ^ " and " ^ string_of_typ (fst (check_expr a2)) ^ " in array"))
                              | _ :: t -> dups t
                            in dups al
                          in
                          check_array li;
                          let checked_li = List.map check_expr li
                          in (Array(fst (List.hd checked_li), List.length checked_li), SArrayLit(checked_li) )
      | MethodCall(cvar, mname, args) ->
      (* check for valid expressions and valid class
       get class type
       check cvar is in the symbol table
       check mname is a valid method of that class type
       check args match
      *)
       let ctype = string_of_typ (type_of_identifier cvar) in
       let cdict = find_class ctype in
       let cmethods = cdict.funcs in
        let check_args_f (fd : fdecl ) =
            let rec comp_args = function
                [], [] -> ()
                | [], _ -> raise (Failure(" Mismatched number of arguments in method " ^ mname ))
                | _, [] -> raise (Failure(" Mismatched number of arguments in method " ^ mname ))
                | (h_arg:: t_arg), (h_form:: t_form) -> if fst (check_expr h_arg) != fst h_form  then
                raise (Failure ("mismatched types of " ^ string_of_typ (fst (check_expr h_arg) ) ^ " and " ^ string_of_typ (fst h_form) ^ " in array"))
                else comp_args (t_arg, t_form)
            in comp_args (args, fd.formals)
          in
            List.map check_args_f cmethods;
             (Obj(ctype), SMethodCall(cvar, mname, List.map check_expr args))
      | Constructor(obj, args) -> let class_dict = find_class obj in
                    let cons_list = class_dict.cons in
                        let check_con_params (my_con: con) =
                            let rec check_con = function
                                [] , [] -> ()
                              | [], _ -> raise (Failure(" Mismatched number of arguments in constructor " ^ obj ))
                              | _, [] -> raise (Failure(" Mismatched number of arguments in constructor " ^ obj ))
                              | (h_arg:: t_arg), (h_form:: t_form) -> if fst (check_expr h_arg) != fst h_form  then
                                raise (Failure ("mismatched types of " ^ string_of_typ (fst (check_expr h_arg) ) ^ " and " ^ string_of_typ (fst h_form) ^ " in array"))
                                else check_con (t_arg, t_form)
                            in check_con (args, my_con.args)
                         in
                          List.map check_con_params cons_list; 
                            (Obj(obj), SConstructor(obj, List.map check_expr args))
                          
    in

    let check_bool_expr e =
      let (t, e') = check_expr e in
      match t with
      | Bool -> (t, e')
      |  _ -> raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
    in

    let rec check_stmt_list =function
        [] -> []
      | s :: sl -> check_stmt s :: check_stmt_list sl
    (* Return a semantically-checked statement i.e. containing sexprs *)
    and check_stmt =function
      (* A block is correct if each statement is correct and nothing
         follows any Return statement.  Nested blocks are flattened. *)
        Block sl -> SBlock (check_stmt_list sl)
      | Expr e -> SExpr (check_expr e)
      | If(e, st1, st2) ->
        SIf(check_bool_expr e, check_stmt st1, check_stmt st2)
      | While(e, st) ->
        SWhile(check_bool_expr e, check_stmt st)
      | For(e1, e2, e3, st) ->
        SFor(check_expr e1, check_bool_expr e2, check_expr e3, check_stmt st)
      | Return e ->
        let (t, e') = check_expr e in
        if t = func.rtyp then SReturn (t, e')
        else raise (
            Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                     string_of_typ func.rtyp ^ " in " ^ string_of_expr e))
    in (* body of check_func *)
    { srtyp = func.rtyp;
      sfname = func.fname;
      sformals = func.formals;
      slocals  = func.locals;
      sbody = check_stmt_list func.body
    }
  in
  let class_names = (List.sort (fun a b -> compare a.cname b.cname) classes)
  in
  let check_classes clist =
    let check_names (cl : cdecl list) =
      let rec dups = function
          [] -> ()
        |	(c1 :: c2 :: _) when c1.cname = c2.cname ->
          raise (Failure ("duplicate class name " ^ c1.cname))
        | _ :: t -> dups t
      in dups class_names
    in
    check_names clist;

    let check_class c =
      check_binds "class" c.vars;
      {
        scmod = c.cmod;
        scname = c.cname;
        svars = c.vars;
        sfuncs = List.map check_func c.funcs
      }
    in
    (List.map check_class clist)
  in
  (check_classes classes, globals, List.map check_func functions)

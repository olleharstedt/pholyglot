(*
 * Module to infer types of local variables
 * Both inferring types of expression, but also iterating the ast to replace Infer_me with proper types.
 *)

open Printf
open Ast
module Log = Dolog.Log

exception Type_error of string

(**
 * Global variable
 * Should only be used by Function_call expression to replace type variables in Function_type
 * TODO: Would this work when wrapping multiple generic functions in one call?
 *)
let t_vars_tbl : (string, typ) Hashtbl.t = Hashtbl.create 10

let typ_of_docblock d : typ = match d with
    | DocParam (_ , t) -> t
    | DocAlloc _ -> raise (Type_error "typ_of_docblock: Can't infer type from DocAlloc")
    | DocVar (_, t) -> t

(**
 *  Travers t and replace Infer_allocation_strategy with strat
 *)
let rec infer_alloc t strat = match t with
    | Class_type (s, Infer_allocation_strategy) -> Class_type (s, strat)
    | List t -> List (infer_alloc t strat)
    | Hash_table (k, v) -> Hash_table (infer_alloc k strat, infer_alloc v strat)
    | t -> t

let rec typ_of_lvalue ns lv : typ = 
    Log.debug "%s %s" "typ_of_lvalue" (show_lvalue lv);
    match lv with
    | Variable id -> 
        begin match Namespace.find_identifier ns id with
        | Some typ -> typ
        | None -> raise (Type_error (sprintf "typ_of_lvalue: Could not find function type %s in namespace" id))
        end
    (* TODO: Access chain like $a->b->c *)
    (* TODO: Property_access is also method call? *)
    (* TODO: id is expression? *)
    | Object_access (id, Property_access prop_name) ->
        let class_type_name = match Namespace.find_identifier ns id with
            | Some (List (Class_type (c, a))) -> "SplDoublyLinkedList"
            | Some (Class_type (c, a)) -> c
            | None -> raise (Type_error (sprintf "typ_of_lvalue: Could not find class type %s in namespace" id))
        in
        let (k, props, methods, builtin_class) = match Namespace.find_class ns class_type_name with
            | Some p -> p
            | None -> raise (Type_error (sprintf "typ_of_lvalue: Found no class declarion %s in namespace" class_type_name))
        in
        match List.find_opt (fun (name, t) -> prop_name = name) props with
            | Some (n, t) -> t
            | None -> raise (Type_error (sprintf "typ_of_lvalue: Could not find property with name %s in class %s" prop_name id))

let rec typ_of_expression (ns : Namespace.t) (expr : expression) : typ = 
    Log.debug "%s %s" "typ_of_expression" (show_expression expr);
    match expr with
    | Num _ -> Int
    | Num_float _ -> Float
    | String s -> String
    | Plus (e, f)
    | Minus (e, f)
    | Times (e, f)
    | Div (e, f) -> 
        let e_typ = typ_of_expression ns e in
        let f_typ = typ_of_expression ns f in
        if e_typ <> f_typ then
            raise (Type_error "typ_of_expression: Mixing float and int in arith expression")
        else
            e_typ
    | Concat (e, f) -> 
        let check e = 
            match typ_of_expression ns e with 
            | String -> () 
            | _ -> raise (Type_error "typ_of_expression: Found non-string in concat")
        in
        check e;
        check f;
        String
    | Parenth e -> typ_of_expression ns e
    | Array_init (Infer_me, length, exprs) ->
        if List.length exprs = 0 then raise (Type_error "array_init cannot be empty list");
        let first_elem = List.nth exprs 0 in
        if List.for_all (fun x -> typ_of_expression ns x = typ_of_expression ns first_elem) exprs then
            (* TODO: Should be able to update this to Dynamic_array *)
            Fixed_array (typ_of_expression ns first_elem, Some (List.length exprs))
        else
            (* TODO: Tuple here *)
            raise (Type_error "not all element in array_init have the same type")
    | Array_init (t, _, _) -> t
    | New (alloc_strat, t, exprs) -> t
    | Clone {variable_name; t; alloc_strat} -> begin
        match Namespace.find_identifier ns variable_name with
        | Some t -> t
        | None -> failwith "Could not find type of clone variable"
    end
    | Clone _ -> failwith "Invalid clone expression: can only clone a variable"
    (* $point[0]-> ? *)
    | Object_access (Array_access (id, _), Property_access prop_name)
    (* $point->x *)
    | Object_access (Variable id, Property_access prop_name) -> begin
        match Namespace.find_identifier ns id with
            | Some (Fixed_array (Class_type (class_type_name, _), _))
            | Some (Class_type (class_type_name, _)) -> begin
                let (k, props, methods, builtin_class) = match Namespace.find_class ns class_type_name with
                    | Some p -> p
                    | None -> raise (Type_error (sprintf "typ_of_expression: Found no class declarion %s in namespace" class_type_name))
                in
                match List.find_opt (fun (name, t) -> prop_name = name) props with
                    | Some (n, t) -> t
                    | None -> raise (Type_error (sprintf "typ_of_expression: Could not find propert with name %s in class %s" prop_name id))
            end
            | None -> raise (Type_error (sprintf "typ_of_expression: Could not find class type %s in namespace" id))
    end
    (* $point->getX() *)
    | Method_call {return_type = Infer_me; method_name; left_hand = Variable class_name}
    (*| Object_access (Array_access (class_name, _), Method_call {return_type = Infer_me; method_name})*)
    | Object_access (Variable class_name, Method_call {return_type = Infer_me; method_name}) -> begin
        let class_type_name = match Namespace.find_identifier ns class_name with
            | Some (Class_type (c, a)) -> c
            | None -> begin
                raise (Type_error (sprintf "typ_of_expression method call: Could not find identifier %s in namespace" class_name))
            end
        in
        let (k, props, methods, builtin_class) = match Namespace.find_class ns class_type_name with
            | Some class_decl -> class_decl
            | None -> raise (Type_error (sprintf "typ_of_expression: Found no class declarion %s in namespace" class_type_name))
        in
        match List.find_opt (fun {name} -> method_name = name) methods with
            | Some {
                function_type = Function_type {return_type; arguments}
            }
                -> return_type
            | None -> raise (Type_error (sprintf "typ_of_expression: Could not find method with name %s in class %s" method_name class_type_name))
    end
    (* TODO: Will this work with chained calls, like $obj->foo()->moo()? *)
    | Object_access (class_id, Method_call {return_type}) -> return_type
    | Variable id -> begin
        match Namespace.find_identifier ns id with 
            | Some p -> p
            | None -> raise (Type_error (sprintf "typ_of_expression: Could not find variable with name %s" id))
    end
    | Function_call (_, id, _) -> begin
        match Namespace.find_function ns id with
        | Some (Function_type {return_type; arguments}) -> return_type
        | Some t -> failwith ("not a function: " ^ show_typ t)
        | _ -> failwith ("found no function declared with name " ^ id)
    end
    (* TODO: Can be both array and hash table access *)
    | Array_or_hash_access (id, expr)
    | Array_access (id, expr) -> begin
        Log.debug "%s %s" "Array_access " id;
        match Namespace.find_identifier ns id with
        | Some (Fixed_array (t, length)) -> t
        | Some (Dynamic_array t) -> t
        | Some (Hash_table (k, v)) -> v
        | _ -> raise (Type_error (sprintf "typ_of_expression: Found no array with id %s in namespace, or could not infer type" id))
    end
    | e -> failwith ("typ_of_expression: missing match case for " ^ (show_expression e))

(**
 * Params always have Memory_polymorph alloc strategy for now.
 *
 * @param t
 * @param def ??
 * @return t
 *)
and infer_arg_typ t def : typ =
    Log.debug "infer_arg_typ %s" (show_typ t);
    match t with
    | Class_type (s, alloc_strat) -> begin
        Log.debug "infer_arg_typ Found Class_type";
        Class_type (s, def)
    end
    | Fixed_array (t, n) -> Fixed_array (infer_arg_typ t def, n)
    | Dynamic_array t -> Dynamic_array (infer_arg_typ t def)
    | List t -> List (infer_arg_typ t def)
    | t -> t


let typ_to_constant (t : Ast.typ) : Ast.expression = match t with
    | Int -> Constant "int"
    | Float -> Constant "float"
    | String -> Constant "string"
    | Class_type (s, _) -> Constant s
    | _ -> raise (Type_error ("typ_to_constant: Not supported type: " ^ show_typ t))

let rec typ_contains_type_variable (t : typ): bool =
    Log.debug "typ_contains_type_variable %s" (show_typ t);
    match t with
    | Function_type {return_type; arguments} ->
        typ_contains_type_variable return_type || List.exists (fun t -> typ_contains_type_variable t) arguments
    | Type_variable _ -> true
    | Dynamic_array t -> typ_contains_type_variable t
    | Fixed_array (t, _) -> typ_contains_type_variable t
    | _ -> false

let rec get_type_variable (t : typ): string option = match t with
    | Function_type {return_type; arguments} -> failwith "get_type_variable: not supported: Function_type"
    | Type_variable s -> Some s
    | Dynamic_array t -> get_type_variable t
    | Fixed_array (t, _) -> get_type_variable t
    | _ -> None

(* Takes a typ and a type variable hashtable and replaces type variables in typ *)
let rec replace_type_variables t : typ =
    Log.debug "replace_type_variables %s" (show_typ t);
    match t with
    | Function_type {return_type; arguments; uses_arena} ->
        Function_type {
            return_type = replace_type_variables return_type;
            arguments   = List.map (fun a -> replace_type_variables a) arguments;
            uses_arena;
        }
    | Type_variable s -> begin
        match Hashtbl.find_opt t_vars_tbl s with
        | Some t -> t
        | None -> raise (Type_error ("Found no resolved type variable with name " ^ s))
    end
    | Dynamic_array t -> replace_type_variables t
    | t -> t
    (*| t -> raise (Type_error ("replace_type_variables: Can only replace type variables in Function_type but got " ^ (show_typ t)))*)

(**
 * Figure out the typ of type variables using namespace, typ and expression list
 * Used for Function_type
 *)
let resolve_type_variable ns t exprs : typ =
    Log.debug "resolve_type_variable %s" (show_typ t);
    match t with
    | Function_type {return_type; arguments} ->
        Hashtbl.clear t_vars_tbl;
        let populate_type_variables = fun arg_t expr ->
            match get_type_variable arg_t with 
            | Some t_var_name -> begin
                let t = typ_of_expression ns expr in
                Log.debug "resolve_type_variable t = %s" (show_typ t);
                Hashtbl.add t_vars_tbl t_var_name t
            end
            | None -> ()
        in
        List.iter2 populate_type_variables arguments exprs;
        replace_type_variables t
    | _ -> raise (Type_error "resolve_type_variable: No Function_type")

(**
 * Replace Infer_me and type variables inside expr using bindings in namespace ns
 *)
let rec infer_expression ns expr : expression = 
    Log.debug "%s %s" "infer_expression" (show_expression expr);
    match expr with
    (* This is allowed to enable infering aliasing, like $b = $a *)
    | Variable id -> Variable id
    | Function_call (Infer_me, name, params) -> begin
        let inf = fun e -> infer_expression ns e in
        let params = List.map inf params in
        match Namespace.find_function ns name with
        | Some (Function_type {return_type; arguments} as fun_t) ->
            if typ_contains_type_variable fun_t then begin
                let resolved_fun_t = resolve_type_variable ns fun_t params in
                Log.debug "resolved_fun_t = %s" (show_typ resolved_fun_t);
                Function_call (resolved_fun_t, name, params)
            end else
                Function_call (fun_t, name, params)
            (* TODO: Type variable here *)
            (*Function_call (Function_type {return_type; arguments}, name, params)*)
        | Some t -> failwith ("not a function: " ^ show_typ t)
        | _ -> failwith ("infer_expression: found no function declared with name " ^ name)
    end
    | Method_call {return_type = Infer_me; method_name; left_hand = Variable object_name; args} as e -> begin
        let t = typ_of_expression ns e in
        Method_call {
            return_type = t;
            method_name;
            left_hand = Variable object_name;
            left_hand_t = typ_of_expression ns (Variable object_name);
            args
        }
    end
    | Object_access (leftside_expr, expr) -> Object_access (infer_expression ns leftside_expr, infer_expression ns expr)
    | Array_init (Infer_me, _, exprs) as e ->
        let length = List.length exprs in
        let inf = fun e -> typ_of_expression ns e in
        let exprs_types = List.map inf exprs in
        let t = typ_of_expression ns e in
        (* TODO: Why is this needed? *)
        let tt = match t with Fixed_array (t, _) -> t in
        Function_call (
            Function_type {
                return_type = t;
                arguments   = Constant :: Int :: exprs_types;
                uses_arena  = false;
            },
            "array_make",
            typ_to_constant tt :: Num length :: exprs
        )
    | Array_or_hash_access (id, expr) as e -> begin
        match Namespace.find_identifier ns id with
            | Some (Fixed_array (t, _))
            | Some (Dynamic_array t) -> begin
                let t = typ_of_expression ns e in
                Function_call (
                    Function_type {
                        return_type = t;
                        arguments   = Constant :: Dynamic_array t :: Int :: [];
                        uses_arena  = false;
                    },
                    "array_get",
                    typ_to_constant t :: Variable id :: expr :: [];
                )
            end
            | Some (Hash_table (k, v)) ->
                (* TODO: Hash key can be more complex expression? *)
                let expr = match expr with
                    | String s -> Coerce (String_gstring, String s)
                    | e -> e
                in
                Log.debug "infer_expression: hash access expr %s " (show_expression expr);
                let t = typ_of_expression ns e in
                Lib_method_call {
                    lvalue = Object_access (id, Property_access "offsetGet");
                    lvalue_t = v;
                    args = [expr];
                }
            | Some t -> failwith ("infer_expression: Faulty type in array/hash access: " ^ show_typ t)
            | None -> failwith ("infer_expression: Could not find identifier " ^ id)
    end 
    | Array_access (id, expr) as e ->
        let t = typ_of_expression ns e in
        Function_call (
            Function_type {
                return_type = t;
                arguments   = Constant :: Dynamic_array t :: Int :: [];
                uses_arena  = false;
            },
            "array_get",
            typ_to_constant t :: Variable id :: expr :: [];
        )
    (* TODO: Memory context = /** @alloc $variable */ *)
    (*| New (alloc_strat, Class_type (class_name, Infer_allocation_strategy), args) -> New (alloc_strat, Class_type (class_name, Boehm), args)*)
    (* TODO: Hard-coded Boehm as default GC *)
    | New (None, t, args) -> New (None, infer_alloc t Boehm, args)

    | Clone {variable_name; t = Infer_me; alloc_strat = None} as e -> begin
        let t = typ_of_expression ns e in
        let alloc_strat = match t with
            | Class_type (_, alloc_strat) -> alloc_strat
            | _ -> failwith "Found no alloc_strat for clone expression"
        in
        Clone {variable_name; t; alloc_strat = Some alloc_strat}
    end
    | Clone _ -> failwith "Not implemented: Infer Clone missing t = Infer_me and alloc_strat = None"

    (*| New (alloc_strat, List (Class_type (class_name, Infer_allocation_strategy)), args) -> New (alloc_strat, List (Class_type (class_name, Boehm)), args)*)
    | List_init t -> List_init t
    | Hash_init t -> Hash_init t
    (* TODO: Be explicit of what to not infer *)
    | e -> e
    (*| e -> failwith ("infer_expression " ^ show_expression expr)*)

let infer_expressions ns exprs =
    let inf = fun e -> infer_expression ns e in
    List.map inf exprs

(**
 * Parse format string from printf and return a list of types
 *)
let infer_printf (s : string) : Ast.typ list =
    Log.debug "infer_printf";
    let s = Str.global_replace (Str.regexp "%%") "" s in
    let regexp = Str.regexp "%[sdf]" in
    let rec get_all_matches i = match Str.search_forward regexp s i with
        | i -> 
            let m = Str.matched_string s in
            (match m with 
                | "%s" -> String_literal
                | "%d" -> Int
                | "%f" -> Float
            ) :: get_all_matches (i + 1)
        | exception Not_found -> []
    in
    get_all_matches 0

(**
 * Returns string list of all type variables in t
 *)
(*
let rec find_all_type_variables t : string list = match t with
    | Function_type {return_type; arguments} ->
        find_all_type_variables return_type @ List.map (fun x -> find_all_type_variables x) arguments
    | Type_variable tv -> [tv]
    | _ -> []
*)

let find_docblock (l : docblock_comment list) (id : string) : docblock_comment option =
    List.find_opt (fun docblock_comment -> match docblock_comment with
        | DocParam (id_, _) -> id = id_
        | _ -> false
    ) l

(**
 * The docblock takes precedence, because it's more precise, unless there's a conflict
 *
 * @param params
 * @param comments
 * @return
 *)
let unify_params_with_docblock (params : param list) (comments : docblock_comment list) : param list =
    (* Are all params represented in the docblock? *)
    let map = (fun p -> 
        match p with
        | RefParam (id, Fixed_array (t, size_option)) ->
            begin match find_docblock comments id with
                | Some (DocParam (_, Dynamic_array (t_))) -> RefParam (id, Dynamic_array (infer_arg_typ t_ Memory_polymorph))
                | None -> p
            end
        | RefParam (id, t) -> RefParam (id, infer_arg_typ t Memory_polymorph)
        | Param (id, t) ->
            begin match find_docblock comments id with
                | Some (DocParam (_, doc_t)) -> Param (id, doc_t)
                | None -> Param (id, infer_arg_typ t Memory_polymorph)
            end
    ) in
    List.map map params

(** Infer typ inside Param/RefParam *)
let infer_arg_typ_param p : param =
    Log.debug "infer_arg_typ_param %s" (show_param p);
    match p with
    | Param (id, t) ->
        let new_t = infer_arg_typ t Memory_polymorph in
        Param (id, new_t)
    | RefParam (id, t) ->
        let new_t = infer_arg_typ t Memory_polymorph in
        RefParam (id, new_t)


(**
 * Infer types inside Ast.statement
 * infer_statement
 *)
let rec infer_stmt (s : statement) (ns : Namespace.t) : statement = 
    Log.debug "infer_stmt: %s" (show_statement s);
    match s with
    | Assignment (Infer_me, Variable id, expr) ->
        (* TODO: Move to infer_expression? *)
        begin match expr with
            | New (Some Arena, _, _) -> ns.uses_arena <- true
            | _ -> ()
        end;
        Log.debug "infer_stmt: assignment to id %s" id;
        let t = typ_of_expression ns expr in
        let expr = infer_expression ns expr in
        let t = replace_type_variables t in
        let t = infer_arg_typ t Boehm in
        Log.debug "id %s typ = %s" id (show_typ t);
        Namespace.add_identifier ns id t;
        Assignment (t, Variable id, expr)
    (* If t is not Infer_me, we have a @var annotation. Note that expr can still contain a @alloc annotation *)
    | Assignment (t, Variable id, New (alloc_opt, t2, [List_init t3])) ->
        if match alloc_opt with | Some Arena -> true | _ -> false then ns.uses_arena <- true;
        (* t can be _partially_ inferred, e.g. missing alloc strat *)
        if t <> Infer_me then begin
            let expr = infer_expression ns (New (alloc_opt, t2, [List_init t3])) in
            let expr_t = typ_of_expression ns expr in
            if t <> expr_t then begin
                (*
                print_endline "383: t <> expr_t";
                print_endline ("t = " ^ show_typ t);
                print_endline ("infer_alloc t Boehm = " ^ show_typ (infer_alloc t Boehm));
                print_endline ("expr_t = " ^ show_typ expr_t);
                *)
            end;
            let new_t = match t, expr_t with 
                | List t, Infer_me -> List (infer_alloc t Boehm)
                | Infer_me, _ -> failwith "Infer_me should not happen here"
                | u, v ->failwith "Could not combine @var and expr type"
            in
            (*
            print_endline ("new_t = " ^ show_typ new_t);
            *)
            let expr = New (alloc_opt, new_t, [List_init new_t]) in
            Namespace.add_identifier ns id new_t;
            Assignment (new_t, Variable id, expr)
        end else
            failwith "infer_stmt: impossible"
    (* Hash_init same logic as List_init? *)
    | Assignment (t, Variable id, New (alloc_opt, t2, [Hash_init t3])) ->
        if match alloc_opt with | Some Arena -> true | _ -> false then ns.uses_arena <- true;
        (* TODO: Code duplication *)
        if t <> Infer_me then begin
            let expr = infer_expression ns (New (alloc_opt, t2, [Hash_init t3])) in
            let expr_t = typ_of_expression ns expr in
            if t <> expr_t then begin
                (*
                print_endline "463: t <> expr_t";
                print_endline ("t = " ^ show_typ t);
                print_endline ("infer_alloc t Boehm = " ^ show_typ (infer_alloc t Boehm));
                print_endline ("expr_t = " ^ show_typ expr_t);
                *)
            end;
            let new_t = match t, expr_t with 
                | Hash_table (k, v), Infer_me -> Hash_table (infer_alloc k Boehm, infer_alloc v Boehm)
                | Infer_me, _ -> failwith "Infer_me should not happen here"
                | u, v ->failwith "Could not combine @var and expr type"
            in
            (*
            print_endline ("new_t = " ^ show_typ new_t);
            *)
            let expr = New (alloc_opt, new_t, [Hash_init new_t]) in
            Namespace.add_identifier ns id new_t;
            Assignment (new_t, Variable id, expr)
        end else
            failwith "infer_stmt: impossible"
    (* TODO: Generalize this with lvalue *)
    (* TODO: variable_name is expression? *)
    | Assignment (Infer_me, Object_access (variable_name, Property_access prop_name), expr) ->
        let t = typ_of_expression ns expr in
        (* Check if class def has property with name and type *)
        let class_name = match Namespace.find_identifier ns variable_name with
            | Some (Class_type (s, alloc_strat)) -> s
            | None -> failwith ("infer_stmt: Could not find identifier " ^ variable_name)
        in
        let (k, props, methods, builtin_class) = match Namespace.find_class ns class_name with
            | Some v -> v
            | None -> failwith ("infer_stmt: Could not find class type " ^ class_name)
        in
        let prop_type = match List.find_opt (fun (prop_name2, p) -> prop_name = prop_name2) props with
            | Some (name, t) -> t
            | None -> failwith ("infer_stmt: Found no class property with name " ^ prop_name)
        in
        if not (prop_type = t) then raise (Type_error
            (
                sprintf
                "Right-hand expression type %s is not the same as the defined property type %s : %s"
                (show_typ t)
                prop_name
                (show_typ prop_type)
            )
        );
        (* TODO: variable_name is expression? *)
        Assignment (typ_of_expression ns expr, Object_access (variable_name, Property_access prop_name), infer_expression ns expr)
    (* printf is hard-coded *)
    (* Head of expressions is always a format string to printf *)
    | Function_call (Infer_me, "printf", String s :: xs) ->
        Log.debug "infer_stmt: printf";
        let expected_types = infer_printf s in
        (* Convert %d to %ld etc for long *)
        let adapted_s = Str.global_replace (Str.regexp "%d") "%ld" s in
        let exprs : expression list = Coerce (String_literal, String adapted_s) :: List.map2 (fun e t -> match e, t with
            (* Match on xs and expected_types to check that it matches *)
            | String s, String_literal -> Coerce (String_literal, e)
            | e, t -> begin
                match typ_of_expression ns e with
                | String -> Coerce (String_literal, infer_expression ns e)
                | expr_typ when expr_typ <> t -> raise (
                    Type_error (
                        sprintf
                        "infer_stmt: Wrong argument given to printf: Got %s but expected %s (expression = %s?)"
                        (show_typ expr_typ)
                        (show_typ t)
                        (show_expression e)
                    )
                )
                | _ -> infer_expression ns e
            end
        ) xs expected_types in
        Function_call (
            Function_type {
                return_type = Void;
                arguments   = String_literal :: expected_types;
                uses_arena  = false;
            },
            "printf",
            exprs
        )
    | Function_call (Infer_me, "printf", _ :: xs) ->
        failwith "infer_stmt: printf must have a string literal as first argument"
    | Function_call (Infer_me, id, e) ->
        let t = match Namespace.find_function ns id with
            | Some (Function_type {return_type; arguments} as t) -> t
            | Some t -> failwith ("not a function: " ^ show_typ t)
            | _ -> failwith ("found no function declared with name " ^ id)
        in
        Function_call (t, id, infer_expressions ns e)
    | Foreach {arr (* Array expression *) ; key; value = Variable value_name; body = stmts} as e -> (
        let t = typ_of_expression ns arr in
        let f = fun s -> infer_stmt s ns in
        (match t with 
            | Fixed_array _
            | List _
            | Dynamic_array _ -> ()
            | _ -> raise (Type_error ("Array given to foreach does not have an array type, but instead " ^ show_typ t))
        );
        (match t with 
            | Fixed_array (array_internal_type, _)
            | Dynamic_array array_internal_type -> (
                (* NB: Since PHP lack block scope, we don't have to clone the namespace or remove variable after *)
                Namespace.add_identifier ns value_name array_internal_type;
                (match key with Some (Variable s) -> Namespace.add_identifier ns s Int | _ -> ());
                let value_typ = typ_of_expression ns (Variable value_name) in
                Foreach {arr; key; value = Variable value_name; value_typ; value_typ_constant = typ_to_constant value_typ; body = List.map f stmts}
            )
            | List array_internal_type -> begin
                Namespace.add_identifier ns value_name array_internal_type;
                (match key with Some (Variable s) -> Namespace.add_identifier ns s Int | _ -> ());
                let value_typ = typ_of_expression ns (Variable value_name) in
                Foreach_list {
                    arr;
                    key;
                    value = Variable value_name;
                    value_typ;
                    body = List.map f stmts;
                }
            end
        )
    )
    | Dowhile {condition; body;} ->
        let inf = fun s -> infer_stmt s ns in
        let new_body = List.map inf body in
        Dowhile {condition = infer_expression ns condition; body = new_body;}
    (* TODO: Can't deal with $arr[0]->push($point) *)
    | Method_call {lvalue = Object_access (id, etc) ; lvalue_t = Infer_me; args} ->
        let lvalue_t = typ_of_expression ns (Variable id) in
        if lvalue_t = Infer_me then failwith ("lvalue_t is still Infer_me after typ_of_lvalue of id " ^ id);
        let builtin_class = begin match lvalue_t with 
            (* List is SplDoublyLinkedList, so always built-in *)
            | List _ -> true
            | Class_type (s, _) -> begin
                match Namespace.find_class ns s with
                | Some (_, _, _, builtin_class) -> builtin_class
                | _ -> failwith ("Found no class " ^ s)
            end
            | _ -> failwith "lvalue_t is not class"
        end
        in
        if builtin_class then
            Lib_method_call {
                lvalue = Object_access (id, etc);
                lvalue_t;
                args;
            }
        else
            Method_call {
                lvalue = Object_access (id, etc);
                lvalue_t;
                args;
            }
    | Hash_set {
        hash_var = Variable name; hash_typ = Infer_me; key; value
    } -> begin
        (* TODO *)
        let t = match Namespace.find_identifier ns name with
            | Some typ -> typ
            | None -> raise (Type_error (sprintf "infer_stmt: Could not find variable %s in namespace" name))
        in
        let key_t   = typ_of_expression ns key in
        let value_t = typ_of_expression ns value in
        (* Check so that key and value is proper type *)
        begin match t with 
            | Hash_table (k, v) -> begin
                if key_t <> k then failwith (
                    sprintf 
                    "Hash table key is wrong type for variable '%s' - found %s but expected %s"
                    name
                    (show_typ key_t)
                    (show_typ k)
                );
                if value_t <> v then failwith (
                    sprintf
                    "Hash table value is wrong type for variable '%s' - found %s but expected %s"
                    name
                    (show_typ value_t)
                    (show_typ v)
                )
            end
            | t -> failwith ("Hash table is not hash table type, but " ^ show_typ t)
        end;
        Hash_set {
            hash_var = Variable name;
            hash_typ = t;
            key;
            value
        }
    end
    | Plusplus e -> Plusplus e
    | Minusminus e -> Minusminus e
    | Pluseq (lv, e) -> Pluseq (lv, e)
    | Minuseq (lv, e) -> Minuseq (lv, e)
    | Return e -> Return e
    | s -> failwith ("Missing match statement in infer_stmt: " ^ show_statement s)

let rec kind_of_typ ns t : kind = match t with
    | Int | Float | Void -> Val
    | String -> Ref
    | Class_type (s, alloc_strat) -> begin
        match Namespace.find_class ns s with
        | Some (Infer_kind, props, methods, builtin_class) -> infer_kind ns Infer_kind props
        | Some (k, _, _, _) -> k
        | None -> failwith ("kind_of_typ: Cannot find class " ^ s)
    end
    | t -> failwith ("kind_of_typ: " ^ show_typ t)

(**
 * @param namespace
 * @param kind
 * @param prop list
 * @return kind
 *)
and infer_kind ns k (props : (string * typ) list) : kind =
    let all_props_are_val props = 
        let l = List.filter (fun (_, t) -> kind_of_typ ns t = Val) props in
        List.length l = List.length props
    in
    match k with
    | Infer_kind -> begin
    if all_props_are_val props then Val else Ref
    end
    | k -> k


(** Check if return type is correct, in relation to declared function type *)
let check_return_type ns stmt typ = 
    match stmt with
    | Return exp ->
        Log.debug "%s %s" "check_return_type" (show_statement stmt);
        let return_type = typ_of_expression ns exp in
        if (kind_of_typ ns return_type) = Ref then raise (Type_error "A function cannot return a Ref kind");
        if compare_typ typ return_type = 0 then
            ()
        else
            failwith (sprintf "Return type %s is not expected type %s" (show_typ return_type) (show_typ typ))
    | _ -> ()
    (* TODO: If, foreach, etc *)

(**
 * Infer and resolve conflicts between docblock, params and function type.
 *)
let unify_params_with_function_type params (Function_type {return_type; arguments; uses_arena}) =
    Log.debug "unify_params_with_function_type";
    let map = (fun param arg ->
        let param = infer_arg_typ_param param in
        Log.debug "unify_params_with_function_type inferred param = %s" (show_param param);
        let arg = infer_arg_typ arg Memory_polymorph in
        Log.debug "unify_params_with_function_type inferred arg = %s" (show_typ arg);
        match param, arg with
        (* Dynamic_array from docblock always wins over non-yet inferred Fixed_array *)
        | RefParam (id, Dynamic_array t), Fixed_array (Infer_me, None) -> begin
            Log.debug "unify_params_with_function_type Picking dynamic_array with typ %s" (show_typ t);
            Dynamic_array (t)
        end
        | _, Fixed_array (Infer_me, _) -> arg
        | Param (_, List t), _ -> List t
        | _, _ -> arg
    ) in
    Function_type {
        return_type;
        arguments = List.map2 map params arguments;
        uses_arena;
    }

(**
 * Replace Infer_allocation_strategy inside docblock.
 *)
let infer_docblock d : docblock_comment =
    match d with
    | DocParam (id, t) -> DocParam (id, infer_arg_typ t Memory_polymorph)

(**
 * Replace Infer_me inside statements in method.
 *)
let infer_method (c_orig : Ast.declaration) meth ns : function_def = match meth with
    | {
        name;
        docblock;
        params;
        stmts;
        function_type = Function_type {return_type; arguments; uses_arena};
    } ->
        let class_name = match c_orig with Class {name;} -> name in
        let params : Ast.param list = unify_params_with_docblock params docblock in
        let ftyp =
            unify_params_with_function_type
            params
            (Function_type {return_type; arguments; uses_arena})
        in
        let ns = Namespace.reset_identifiers ns in
        (* Add method args to namespace *)
        List.iter (fun p -> match p with
            | Param (id, typ)
            | RefParam (id, typ) -> Namespace.add_identifier ns id typ
            | C_only_param (id, typ) -> failwith "here"
        ) params;
        (* TODO: Does alloc strat matter here? *)
        Namespace.add_identifier ns "this" (Class_type (class_name, Boehm));
        let inf = fun s -> infer_stmt s ns in
        let new_stmts = List.map inf stmts in
        {name; docblock; params; stmts = new_stmts; function_type = ftyp}

let infer_declaration decl ns : declaration = 
    Log.debug "infer_declaration %s" (show_declaration decl);
    match decl with
    (*
    | Function of function_name * param list * statement list * typ
    | Struct of struct_name * struct_field list
    *)
    | Function {
        name;
        docblock;
        params;
        stmts;
        function_type = Function_type {return_type; arguments; uses_arena};
    } ->
        if (kind_of_typ ns return_type) = Ref then raise (Type_error "A function cannot have a Ref kind as return type");
        let docblock = List.map infer_docblock docblock in
        let params = unify_params_with_docblock params docblock in
        let ftyp =
            unify_params_with_function_type
            params
            (Function_type {return_type; arguments; uses_arena})
        in
        Log.debug "infer_declaration: ftyp = %s" (show_typ ftyp);
        Namespace.add_function_type ns name ftyp;
        let ns = Namespace.reset_identifiers ns in
        Namespace.add_params ns params;
        ns.uses_arena <- false;
        let inf = fun s -> infer_stmt s ns in
        let new_stmts = List.map inf stmts in
        let new_stmts = if ns.uses_arena then begin
            (*print_endline "here!";*)
            Init_arena :: new_stmts
        end else
            new_stmts
        in
        let _ = List.map (fun s -> check_return_type ns s return_type) new_stmts in
        let Function_type r = ftyp in
        let ftyp = Function_type {r with uses_arena = ns.uses_arena} in
        Function {
            name;
            docblock;
            params;
            stmts = new_stmts;
            function_type = ftyp;
        }
    | Function {function_type = ftyp} -> failwith ("infer_declaration function typ " ^ show_typ ftyp)
    | Class {name; kind; properties = props; methods; builtin_class} as c_orig when kind = Infer_kind -> 
        (* Temporary class type during inference *)
        Namespace.add_class_type ns c_orig;
        let k = infer_kind ns Infer_kind props in
        let methods = List.map (fun m -> infer_method c_orig m ns) methods in
        let c = Class {name; kind = k; properties = props; methods; builtin_class} in
        Namespace.remove_class_type ns c;
        Namespace.add_class_type ns c;
        c
    | Class {name; kind; properties; methods; builtin_class} -> failwith ("infer_declaration: Class with kind " ^ show_kind kind ^ " " ^ name)

let run (ns : Namespace.t) (p : program): program = 
    Log.debug "Infer.run";
    match p with
    | Declaration_list decls -> Declaration_list (List.map (fun d -> infer_declaration d ns) decls)

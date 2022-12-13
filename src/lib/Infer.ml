(*
 * Module to infer types of local variables
 * Both inferring types of expression, but also iterating the ast to replace Infer_me with proper types.
 *)

open Printf
open Ast
module Log = Dolog.Log

exception Type_error of string

let rec typ_of_lvalue ns lv : typ = 
    Log.debug "%s %s" "typ_of_lvalue" (show_lvalue lv);
    match lv with
    | Variable id -> 
        begin match Namespace.find_identifier ns id with
        | Some typ -> typ
        | None -> raise (Type_error (sprintf "typ_of_lvalue: Could not find function type %s in namespace" id))
        end
    (* TODO: Access chain like $a->b->c *)
    (* TODO: id is expression? *)
    | Object_access (id, Property_access prop_name) ->
        let class_type_name = match Namespace.find_identifier ns id with
            | Some (Class_type (c)) -> c
            | None -> raise (Type_error (sprintf "typ_of_lvalue: Could not find class type %s in namespace" id))
        in
        let (k, props, methods) = match Namespace.find_class ns class_type_name with
            | Some p -> p
            | None -> raise (Type_error (sprintf "typ_of_lvalue: Found no class declarion %s in namespace" class_type_name))
        in
        match List.find_opt (fun (name, t) -> prop_name = name) props with
            | Some (n, t) -> t
            | None -> raise (Type_error (sprintf "typ_of_lvalue: Could not find propert with name %s in class %s" prop_name id))

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
    | New (t, exprs) -> t
    (* $point->[0] ? *)
    | Object_access (Array_access (id, _), Property_access prop_name)
    (* $point->x *)
    | Object_access (Variable id, Property_access prop_name) -> begin
        match Namespace.find_identifier ns id with
            | Some (Fixed_array (Class_type class_type_name, _))
            | Some (Class_type class_type_name) -> begin
                let (k, props, methods) = match Namespace.find_class ns class_type_name with
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
            | Some (Class_type (c)) -> c
            | None -> begin
                raise (Type_error (sprintf "typ_of_expression method call: Could not find identifier %s in namespace" class_name))
            end
        in
        let (k, props, methods) = match Namespace.find_class ns class_type_name with
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
    | Array_access (id, expr) -> begin
        Log.debug "%s %s" "Array_access " id;
        match Namespace.find_identifier ns id with
        | Some (Fixed_array (t, length)) -> t
        | _ -> raise (Type_error (sprintf "typ_of_expression: Found no array with id %s, or could not infer type" id))
    end
    | e -> failwith ("typ_of_expression: " ^ (show_expression e))

let typ_to_constant (t : Ast.typ) : Ast.expression = match t with
    | Int -> Constant "int"
    | String -> Constant "string"
    | Float -> Constant "float"
    | Class_type s -> Constant s
    | _ -> raise (Type_error ("typ_to_constant: Not supported type: " ^ show_typ t))

let rec infer_expression ns expr = 
    Log.debug "%s %s" "infer_expression" (show_expression expr);
    match expr with
    (* This is allowed to enable infering aliasing, like $b = $a *)
    | Variable id -> Variable id
    | Function_call (Infer_me, name, params) -> begin
        let inf = fun e -> infer_expression ns e in
        let params = List.map inf params in
        match Namespace.find_function ns name with
        | Some (Function_type {return_type; arguments}) -> Function_call (Function_type {return_type; arguments}, name, params)
        | Some t -> failwith ("not a function: " ^ show_typ t)
        | _ -> failwith ("infer_expression: found no function declared with name " ^ name)
    end
    | Method_call {return_type = Infer_me; method_name; left_hand = Variable object_name; args} as e -> begin
        let t = typ_of_expression ns e in
        Method_call {return_type = t; method_name; left_hand = Variable object_name; args}
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
                arguments = Constant :: Int :: exprs_types;
            },
            "array_make",
            typ_to_constant tt :: Num length :: exprs
        )
    | Array_access (id, expr) as e ->
        let t = typ_of_expression ns e in
        Function_call (
            Function_type {
                return_type = t;
                arguments = Constant :: Dynamic_array t :: Int :: [];
            },
            "array_get",
            typ_to_constant t :: Variable id :: expr :: [];
        )
    | e -> e
    (*| e -> failwith ("infer_expression " ^ show_expression expr)*)

let infer_expressions ns exprs =
    let inf = fun e -> infer_expression ns e in
    List.map inf exprs

(** Parse format string from printf etc *)
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
 * Infer types inside Ast.statement
 *)
let infer_stmt (s : statement) (ns : Namespace.t) : statement = 
    Log.debug "%s %s" "infer_stmt" (show_statement s);
    match s with
    | Assignment (Infer_me, Variable id, expr) ->
        Log.debug "%s %s" "assignment " id;
        let t = typ_of_expression ns expr in
        Namespace.add_identifier ns id t;
        Assignment (t, Variable id, infer_expression ns expr)
    (* TODO: Generalize this with lvalue *)
    (* TODO: variable_name is expression? *)
    | Assignment (Infer_me, Object_access (variable_name, Property_access prop_name), expr) ->
        let t = typ_of_expression ns expr in
        (* Check if class def has property with name and type *)
        let class_name = match Namespace.find_identifier ns variable_name with
            | Some (Class_type s) -> s
            | None -> failwith ("infer_stmt: Could not find identifier " ^ variable_name)
        in
        let (k, props, methods) = match Namespace.find_class ns class_name with
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
        let expected_types = infer_printf s in
        let exprs : expression list = Coerce (String_literal, String s) :: List.map2 (fun e t -> match e, t with
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
        Function_call (Function_type {return_type = Void; arguments = String_literal :: expected_types}, "printf", exprs)
    | Function_call (Infer_me, "printf", _ :: xs) ->
        failwith "infer_stmt: printf must have a string literal as first argument"
    | Function_call (Infer_me, id, exprs) ->
        begin match Namespace.find_identifier ns id with
        | Some fun_type -> Function_call (fun_type, id, infer_expressions ns exprs)
        | None -> raise (Type_error (sprintf "infer_stmt: Could not find function type %s in namespace" id))
        end
    | s -> s

let rec kind_of_typ ns t : kind = match t with
    | Int | Float | Void -> Val
    | String -> Ref
    | Class_type s -> begin
        match Namespace.find_class ns s with
        | Some (Infer_kind, props, methods) -> infer_kind ns Infer_kind props
        | Some (k, _, _) -> k
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

let find_docblock (l : docblock_comment list) (id : string) : docblock_comment option =
    List.find_opt (fun docblock_comment -> match docblock_comment with
        | DocParam (id_, _) -> id = id_
        | _ -> false
    ) l

(**
 * docblock takes precedence, because it's more precise, unless there's a conflict
 *)
let unify_params_with_docblock (params : param list) (comments : docblock_comment list) : param list =
    (* Are all params represented in the docblock? *)
    let map = (fun p -> match p with
        | RefParam (id, Fixed_array (t, size_option)) ->
            begin match find_docblock comments id with
                | Some (DocParam (_, Dynamic_array (t_))) -> RefParam (id, Dynamic_array t_)
                | None -> p
            end
        | _ -> p
    ) in
    List.map map params

(**
 * Infer and resolve conflicts between docblock, params and function type.
 *)
let unify_params_with_function_type params (Function_type {return_type; arguments}) =
    let map = (fun param arg ->
        match param, arg with
        (* Dynamic_array from docblock always wins over non-yet inferred Fixed_array *)
        | RefParam (id, Dynamic_array t), Fixed_array (Infer_me, None) -> Dynamic_array t
        | _, Fixed_array (Infer_me, _) -> arg
        | _, _ -> arg
    ) in
    Function_type {
        return_type;
        arguments = List.map2 map params arguments;
    }

let infer_method meth ns : function_def = match meth with
    | {
        name;
        docblock;
        params;
        stmts;
        function_type = Function_type {return_type; arguments};
    } ->
        let params = unify_params_with_docblock params docblock in
        let ftyp =
            unify_params_with_function_type
            params
            (Function_type {return_type; arguments})
        in
        let ns = Namespace.reset_identifiers ns in
        let inf = fun s -> infer_stmt s ns in
        let new_stmts = List.map inf stmts in
        {name; docblock; params; stmts = new_stmts; function_type = ftyp}

let infer_declaration decl ns : declaration = 
    Log.debug "%s %s" "infer_declaration" (show_declaration decl);
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
        function_type = Function_type {return_type; arguments};
    } ->
        if (kind_of_typ ns return_type) = Ref then raise (Type_error "A function cannot have a Ref kind as return type");
        let params = unify_params_with_docblock params docblock in
        let ftyp =
            unify_params_with_function_type
            params
            (Function_type {return_type; arguments})
        in
        Namespace.add_function_type ns name ftyp;
        let ns = Namespace.reset_identifiers ns in
        Namespace.add_params ns params;
        let inf = fun s -> infer_stmt s ns in
        let new_stmts = List.map inf stmts in
        let _ = List.map (fun s -> check_return_type ns s return_type) new_stmts in
        Function {name; docblock; params; stmts = new_stmts; function_type = ftyp}
    | Function {function_type = ftyp} -> failwith ("infer_declaration function typ " ^ show_typ ftyp)
    | Class {name; kind; properties = props; methods} when kind = Infer_kind -> 
        let k = infer_kind ns Infer_kind props in
        let methods = List.map (fun m -> infer_method m ns) methods in
        let c = Class {name; kind = k; properties = props; methods} in
        Namespace.add_class_type ns c;
        c
    | Class {name; kind; properties; methods} -> failwith ("infer_declaration: Class with kind " ^ show_kind kind ^ " " ^ name)

let run (ns : Namespace.t) (p : program): program = 
    Log.debug "Infer.run";
    match p with
    | Declaration_list decls -> Declaration_list (List.map (fun d -> infer_declaration d ns) decls)

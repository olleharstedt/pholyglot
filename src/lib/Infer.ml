(* Module to infer types of local variables *)

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
    | Object_access (id, Property_access prop_name) ->
        let class_type_name = match Namespace.find_identifier ns id with
            | Some (Class_type (c)) -> c
            | None -> raise (Type_error (sprintf "typ_of_lvalue: Could not find class type %s in namespace" id))
        in
        let (k, props) = match Namespace.find_class ns class_type_name with
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
    | String s -> String
    | Plus (e, f)
    | Minus (e, f)
    | Times (e, f)
    | Div (e, f) -> 
        let check e = 
            match typ_of_expression ns e with 
            | Int -> () 
            | _ -> raise (Type_error "typ_of_expression: Found non-int in arith")
        in
        check e;
        check f;
        Int
    | Concat (e, f) -> 
        let check e = 
            match typ_of_expression ns e with 
            | String -> () 
            | _ -> raise (Type_error "typ_of_expression: Found non-string in concat")
        in
        check e;
        check f;
        String
    | Array_init (exprs) ->
        if List.length exprs = 0 then raise (Type_error "array_init cannot be empty list");
        let first_elem = List.nth exprs 0 in
        if List.for_all (fun x -> typ_of_expression ns x = typ_of_expression ns first_elem) exprs then
            (* TODO: Should be able to update this to Dynamic_array *)
            Fixed_array (typ_of_expression ns first_elem, List.length exprs)
        else
            (* TODO: Tuple here *)
            raise (Type_error "not all element in array_init have the same type")
    | New (t, exprs) -> t
    | Object_access (id, Property_access prop_name) -> begin
        let class_type_name = match Namespace.find_identifier ns id with
            | Some (Class_type (c)) -> c
            | None -> raise (Type_error (sprintf "typ_of_expression: Could not find class type %s in namespace" id))
        in
        let (k, props) = match Namespace.find_class ns class_type_name with
            | Some p -> p
            | None -> raise (Type_error (sprintf "typ_of_expression: Found no class declarion %s in namespace" class_type_name))
        in
        match List.find_opt (fun (name, t) -> prop_name = name) props with
            | Some (n, t) -> t
            | None -> raise (Type_error (sprintf "typ_of_expression: Could not find propert with name %s in class %s" prop_name id))
    end
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
        match Namespace.find_identifier ns id with
        | Some (Fixed_array (t, length)) -> t
        | _ -> raise (Type_error (sprintf "typ_of_expression: Found no array with id %s, or could not infer type" id))
    end
    | e -> failwith ("typ_of_expression: " ^ (show_expression e))

let infer_expression ns expr = 
    Log.debug "%s %s" "infer_expression" (show_expression expr);
    match expr with
    (* This is allowed to enable infering aliasing, like $b = $a *)
    | Variable id -> Variable id
    | Function_call (Infer_me, name, params) -> begin
        match Namespace.find_function ns name with
        | Some (Function_type {return_type; arguments}) -> Function_call (Function_type {return_type; arguments}, name, params)
        | Some t -> failwith ("not a function: " ^ show_typ t)
        | _ -> failwith ("infer_expression: found no function declared with name " ^ name)
    end
    | e -> e
    (*| e -> failwith ("infer_expression " ^ show_expression expr)*)

(** Parse format string from printf etc *)
let infer_printf (s : string) : Ast.typ list =
    Log.debug "infer_printf";
    let s = Str.global_replace (Str.regexp "%%") "" s in
    let regexp = Str.regexp "%[sd]" in
    let rec get_all_matches i = match Str.search_forward regexp s i with
        | i -> 
            let m = Str.matched_string s in
            (match m with 
                | "%s" -> String_literal
                | "%d" -> Int
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
        let t = typ_of_expression ns expr in
        Namespace.add_identifier ns id t;
        Assignment (typ_of_expression ns expr, Variable id, infer_expression ns expr)
    (* TODO: Generalize this with lvalue *)
    | Assignment (Infer_me, Object_access (variable_name, Property_access prop_name), expr) ->
        let t = typ_of_expression ns expr in
        (* Check if class def has property with name and type *)
        let class_name = match Namespace.find_identifier ns variable_name with
            | Some (Class_type s) -> s
            | None -> failwith ("infer_stmt: Could not find identifier " ^ variable_name)
        in
        let (k, props) = match Namespace.find_class ns class_name with
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
        Assignment (typ_of_expression ns expr, Object_access (variable_name, Property_access prop_name), expr)
    (* printf is hard-coded *)
    (* Head of expressions is always a format string to printf *)
    | Function_call (Infer_me, "printf", String s :: xs) ->
        let expected_types = infer_printf s in
        let exprs : expression list = Coerce (String_literal, String s) :: List.map2 (fun e t -> match e, t with
            | String s, String_literal -> Coerce (String_literal, e)
            | e, t -> begin
                let expr_typ = typ_of_expression ns e in
                if expr_typ <> t then raise (
                    Type_error (
                        sprintf
                        "Wrong argument given to printf: Got %s but expected %s"
                        (show_typ expr_typ)
                        (show_typ t)
                    )
                );
                e
            end
        ) xs expected_types in
        Function_call (Function_type {return_type = Void; arguments = String_literal :: expected_types}, "printf", exprs)
    | Function_call (Infer_me, "printf", _ :: xs) ->
        failwith "infer_stmt: printf must have a string literal as first argument"
    | Function_call (Infer_me, id, exprs) ->
        begin match Namespace.find_identifier ns id with
        | Some fun_type -> Function_call (fun_type, id, exprs)
        | None -> raise (Type_error (sprintf "infer_stmt: Could not find function type %s in namespace" id))
        end
    | s -> s

let rec kind_of_typ ns t : kind = match t with
    | Int | Void -> Val
    | String -> Ref
    | Class_type s -> begin
        match Namespace.find_class ns s with
        | Some (Infer_kind, props) -> infer_kind ns Infer_kind props
        | Some (k, _) -> k
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

let infer_declaration decl ns : declaration = 
    Log.debug "%s %s" "infer_declaration" (show_declaration decl);
    match decl with
    (*
    | Function of function_name * param list * statement list * typ
    | Struct of struct_name * struct_field list
    *)
    | Function (name, params, stmts, Function_type {return_type; arguments}) ->
            if (kind_of_typ ns return_type) = Ref then raise (Type_error "A function cannot have a Ref kind as return type");
        let typ = Function_type {return_type; arguments} in
        Namespace.add_function_type ns name typ;
        let ns = Namespace.reset_identifiers ns in
        Namespace.add_params ns params;
        let inf = fun s -> infer_stmt s ns in
        let new_stmts = List.map inf stmts in
        let _ = List.map (fun s -> check_return_type ns s return_type) new_stmts in
        Function (name, params, new_stmts, typ)
    | Function (_, _, _, typ) -> failwith ("infer_declaration function typ " ^ show_typ typ)
    | Class (name, Infer_kind, props) -> 
        let k = infer_kind ns Infer_kind props in
        let c = Class (name, k, props) in
        Namespace.add_class_type ns c;
        c
    | Class (name, k, props) -> failwith ("infer_declaration: Class with kind " ^ show_kind k ^ " " ^ name)

let run (ns : Namespace.t) (p : program): program = 
    Log.debug "Infer.run";
    match p with
    | Declaration_list decls -> Declaration_list (List.map (fun d -> infer_declaration d ns) decls)

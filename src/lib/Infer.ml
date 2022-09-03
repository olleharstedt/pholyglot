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
            | Some (Class_type c) -> c
            | None -> raise (Type_error (sprintf "typ_of_lvalue: Could not find class type %s in namespace" id))
        in
        let props = match Namespace.find_class ns class_type_name with
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
            | Some (Class_type c) -> c
            | None -> raise (Type_error (sprintf "typ_of_expression: Could not find class type %s in namespace" id))
        in
        let props = match Namespace.find_class ns class_type_name with
            | Some p -> p
            | None -> raise (Type_error (sprintf "typ_of_expression: Found no class declarion %s in namespace" class_type_name))
        in
        match List.find_opt (fun (name, t) -> prop_name = name) props with
            | Some (n, t) -> t
            | None -> raise (Type_error (sprintf "typ_of_expression: Could not find propert with name %s in class %s" prop_name id))
    end
    | Variable id -> begin
        let var_typ_name = match Namespace.find_identifier ns id with 
            | Some p -> p
            | None -> raise (Type_error (sprintf "typ_of_expression: Could not find variable with name %s" id))
        in
        Int
    end

    | e -> failwith ("typ_of_expression: " ^ (show_expression e))

let infer_expression expr = 
    Log.debug "%s %s" "infer_expression" (show_expression expr);
    match expr with
    (* TODO: Namespace *)
    | Variable id -> raise (Type_error ("Can't infer type of variable " ^ id))

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
            print_endline id;
        let t = typ_of_expression ns expr in
        Namespace.add_identifier ns id t;
        Assignment (typ_of_expression ns expr, Variable id, expr)
    | Assignment (Infer_me, id, expr) ->
        let t = typ_of_expression ns expr in
        Assignment (typ_of_expression ns expr, id, expr)
    (* printf is hard-coded *)
    (* Head of expressions is always a format string to printf *)
    | Function_call (Infer_me, "printf", String s :: xs) ->
        let expected_types = infer_printf s in
        let exprs : expression list = Coerce (String_literal, String s) :: List.map2 (fun e t -> match e, t with
            | String s, String_literal -> Coerce (String_literal, e)
            | e, t -> e
        ) xs expected_types in
        Function_call (Function_type (Void, String_literal :: expected_types), "printf", exprs)
    | Function_call (Infer_me, "printf", _ :: xs) ->
        failwith "infer_stmt: printf must have a string literal as first argument"
    | Function_call (Infer_me, id, exprs) ->
        begin match Namespace.find_identifier ns id with
        | Some fun_type -> Function_call (fun_type, id, exprs)
        | None -> raise (Type_error (sprintf "infer_stmt: Could not find function type %s in namespace" id))
        end
    | s -> s

(** Check if return type is correct, in relation to declared function type *)
let check_return_type ns stmt typ = 
    Log.debug "check_return_type";
    match stmt with
    | Return exp -> if compare_typ typ (typ_of_expression ns exp) = 0 then () else failwith "mo"
    | _ -> ()
    (* TODO: If, foreach, etc *)

let infer_declaration decl ns : declaration = 
    Log.debug "%s %s" "infer_declaration" (show_declaration decl);
    match decl with
    (*
    | Function of function_name * param list * statement list * typ
    | Struct of struct_name * struct_field list
    *)
    | Function (name, params, stmts, typ) ->
        let ns = Namespace.reset_identifiers ns in
        let inf = fun s -> infer_stmt s ns in
        let _ = List.map (fun s -> check_return_type ns s typ) stmts in
        Function (name, params, List.map inf stmts, typ)
    | Class (name,  props) as c -> 
        Namespace.add_class_type ns c;
        c

let run (ns : Namespace.t) (p : program): program = 
    Log.debug "Infer.run";
    match p with
    | Declaration_list decls -> Declaration_list (List.map (fun d -> infer_declaration d ns) decls)

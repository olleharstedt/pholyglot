(* Module to infer types of local variables *)

open Printf
open Ast

exception Type_error of string

let rec typ_of_expression : (expression -> typ) = function
    | Num _ -> Int
    | String s -> String
    | Plus (e, f)
    | Minus (e, f)
    | Times (e, f)
    | Div (e, f) -> 
        let check e = 
            match typ_of_expression e with 
            | Int -> () 
            | _ -> raise (Type_error "typ_of_expression: Found non-int in arith")
        in
        check e;
        check f;
        Int
    | Concat (e, f) -> 
        let check e = 
            match typ_of_expression e with 
            | String -> () 
            | _ -> raise (Type_error "typ_of_expression: Found non-string in concat")
        in
        check e;
        check f;
        String
    | Array_init (exprs) ->
        if List.length exprs = 0 then raise (Type_error "array_init cannot be empty list");
        let first_elem = List.nth exprs 0 in
        if List.for_all (fun x -> typ_of_expression x = typ_of_expression first_elem) exprs then
            (* TODO: Should be able to update this to Dynamic_array *)
            Fixed_array (typ_of_expression first_elem, List.length exprs)
        else
            (* TODO: Tuple here *)
            raise (Type_error "not all element in array_init have the same type")

let infer_expression : (expression -> typ) = function
    (* TODO: Namespace *)
    | Variable id -> raise (Type_error ("Can't infer type of variable " ^ id))

(** Parse format string from printf etc *)
let infer_printf (s : string) : Ast.typ list =
    let s = Str.global_replace (Str.regexp "%%") "" s in
    let regexp = Str.regexp "%[sd]" in
    let rec get_all_matches i = match Str.search_forward regexp s i with
        | i -> 
            let m = Str.matched_string s in
            (match m with 
                | "%s" -> Some String_literal
                | "%d" -> Some Int
            ) :: get_all_matches (i + 1)
        | exception Not_found -> []
    in
    let ts = get_all_matches 0 in
    List.filter_map (fun s -> s) ts

(**
 * Infer types inside Ast.statement
 *)
let infer_stmt (s : statement) (namespace : Namespace.t) : statement = match s with
    | Assignment (Infer_me, id, expr) -> Assignment (typ_of_expression expr, id, expr)
    (* printf is hard-coded *)
    | Function_call (Infer_me, "printf", exprs) ->
        begin match exprs with
        | String s :: xs -> 
            let expected_types = infer_printf s in
            (*
            let head = List.hd exprs in
            let tail = List.tl exprs in
            (* Head of exprs is always a format string to printf *)
            let exprs = head :: List.map2 (fun e t -> match e, t with
                | String s, String_literal -> Coerce (String_literal, e)
                | e, t -> failwith (show_expression e)
            ) tail expected_types
            in
            *)
            let exprs = Coerce (String_literal, String s) :: xs in
            Function_call (Function_type (Void, expected_types), "printf", exprs)
        end
    | Function_call (Infer_me, id, exprs) ->
        begin match Namespace.find namespace id with
        | Some fun_type -> Function_call (fun_type, id, exprs)
        | None -> raise (Type_error (sprintf "Could not find function type %s in namespace" id))
        end
    | s -> s

let infer_declaration : (declaration -> declaration) = function
    (*
    | Function of function_name * param list * statement list * typ
    | Struct of struct_name * struct_field list
    *)
    | Function (name, params, stmts, typ) ->
        let namespace = Namespace.create () |> Namespace.populate in
        let inf = fun s -> infer_stmt s namespace in
        Function (name, params, List.map inf stmts, typ)
    | d -> d

let run : (program -> program) = function
    | Declaration_list decls -> Declaration_list (List.map infer_declaration decls)

(* Module to infer types of local variables *)

exception Type_error of string

let rec typ_of_expression : (Ast.expression -> Ast.typ) = function
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
            Fixed_array (typ_of_expression first_elem)
        else
            (* TODO: Tuple here *)
            raise (Type_error "not all element in array_init have the same type")

let infer_expression : (Ast.expression -> Ast.typ) = function
    (* TODO: Namespace *)
    | Variable id -> raise (Type_error ("Can't infer type of variable " ^ id))
    | e -> typ_of_expression e

let infer_stmt : (Ast.statement -> Ast.statement) = function
    | Assignment (Infer_me, id, expr) -> Assignment (infer_expression expr, id, expr)
    | s -> s

let infer_declaration : (Ast.declaration -> Ast.declaration) = function
    (*
    | Function of function_name * param list * statement list * typ
    | Struct of struct_name * struct_field list
    *)
    | Function (name, params, stmts, typ) -> Function (name, params, List.map infer_stmt stmts, typ)
    | d -> d

let run : (Ast.program -> Ast.program) = function
    | Declaration_list decls -> Declaration_list (List.map infer_declaration decls)

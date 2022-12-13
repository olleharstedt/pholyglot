open Printf

exception Transpile_error of string

let kind_to_pholyglot k = match k with
    | Ast.Ref -> Pholyglot_ast.Ref
    | Ast.Val -> Pholyglot_ast.Val

(** Transpile from Pholly AST to Pholyglot AST *)
let rec typ_to_pholyglot (t : Ast.typ) : Pholyglot_ast.typ = match t with
    | Int -> Pholyglot_ast.Int
    | Float -> Pholyglot_ast.Float
    | String -> Pholyglot_ast.String
    | String_literal -> Pholyglot_ast.String_literal
    | Constant -> Pholyglot_ast.Constant
    | Fixed_array (t, Some n) -> Pholyglot_ast.Fixed_array ((typ_to_pholyglot t), n)
    | Dynamic_array t -> Pholyglot_ast.Dynamic_array (typ_to_pholyglot t)
    | Fixed_array (_, None) -> failwith "typ_to_pholyglot: array has size None, not fully inferred"
    | Void -> Pholyglot_ast.Void
    | Function_type {return_type; arguments} -> Pholyglot_ast.Function_type {return_type = typ_to_pholyglot return_type; arguments = List.map typ_to_pholyglot arguments}
    (** TODO: Should we infer types before moving to Pholyglot_ast? *)
    | Infer_me -> failwith "Infer before transpiling"
    | Class_type (n) -> Pholyglot_ast.Class_type (n)
    | t -> raise (Transpile_error ("typ_to_pholyglot: " ^ Ast.show_typ t))

let typ_to_pholyglot_constant (t : Ast.typ) : Pholyglot_ast.expression = match t with
    | Int -> Constant "int"
    | String -> Constant "string"
    | Float -> Constant "float"
    | Class_type s -> Constant s
    | _ -> raise (Transpile_error ("typ_to_pholyglot_constant: Not supported type"))

let param_to_pholyglot (p: Ast.param) : Pholyglot_ast.param = match p with
    | Param (id, typ) -> Pholyglot_ast.Param (id, typ_to_pholyglot typ)
    | RefParam (id, typ) -> Pholyglot_ast.RefParam (id, typ_to_pholyglot typ)
    | t -> raise (Transpile_error ("param_to_pholyglot: " ^ Ast.show_param t))

let rec lvalue_to_pholyglot lvalue = match lvalue with
    | Ast.Variable id -> Pholyglot_ast.Variable id
    | Ast.Property_access class_property_name -> Pholyglot_ast.Property_access class_property_name
    | Ast.Function_call (typ, id, exprs) ->
        let id = if id = "printf" then "pprintf" else id in
        Pholyglot_ast.Function_call (typ_to_pholyglot typ, id, List.map expression_to_pholyglot exprs)
    | Ast.Object_access (expr, lvalue) -> Pholyglot_ast.Object_access (expression_to_pholyglot expr, lvalue_to_pholyglot lvalue)
    | _ -> failwith (sprintf "lvalue_to_pholyglot lvalue = %s" (Ast.show_expression lvalue))

and expression_to_pholyglot (exp : Ast.expression) : Pholyglot_ast.expression = match exp with
    | String s -> Pholyglot_ast.String s
    | Constant s -> Pholyglot_ast.Constant s
    | Num i -> Pholyglot_ast.Num i
    | Num_float f -> Pholyglot_ast.Num_float f
    | Plus (i, j) -> Pholyglot_ast.Plus (expression_to_pholyglot i, expression_to_pholyglot j)
    | Minus (i, j) -> Pholyglot_ast.Minus (expression_to_pholyglot i, expression_to_pholyglot j)
    | Times (i, j) -> Pholyglot_ast.Times (expression_to_pholyglot i, expression_to_pholyglot j)
    | Div (i, j) -> Pholyglot_ast.Div (expression_to_pholyglot i, expression_to_pholyglot j)
    | Concat (s, t) -> Pholyglot_ast.Concat (expression_to_pholyglot s, expression_to_pholyglot t)
    | Variable id -> Pholyglot_ast.Variable id
    | Array_init (Infer_me, _, _) -> raise (Transpile_error "Array_init: Infer before transpiling")
    | Array_init (_, None, _) -> raise (Transpile_error "Array_init: Array init has no inferred length")
    | Array_init _ -> raise (Transpile_error "Array_init: This should be a function call to array_make")
    (* Array init must be converted to Function_call already during infer to carry length *)
    (*
    | Ast.Array_init (Fixed_array (typ, _), Some length, exprs) ->
        let t = typ_to_pholyglot typ in
        Pholyglot_ast.Function_call (
            Function_type {
                return_type = Fixed_array (t, length);
                arguments = List.map Infer.typ_of_expression exprs;
            },
            "array_make",
            typ_to_pholyglot_constant typ :: Num length :: List.map expression_to_pholyglot exprs
        )
        *)
    (*| Array_access (id, expr) -> Pholyglot_ast.Array_access (id, expression_to_pholyglot expr)*)
    | Array_access _ -> raise (Transpile_error "Array_access: This should be a function call to array_get")
    | Function_call (typ, id, exprs) ->
        let id = if id = "printf" then "pprintf" else id in
        Pholyglot_ast.Function_call (typ_to_pholyglot typ, id, List.map expression_to_pholyglot exprs)
    | Coerce (t, e) -> Pholyglot_ast.Coerce (typ_to_pholyglot t, expression_to_pholyglot e)
    | Object_access (expr, lvalue) -> Pholyglot_ast.Object_access (expression_to_pholyglot expr, expression_to_pholyglot lvalue)
    | Property_access class_property_name -> Pholyglot_ast.Property_access class_property_name
    | Method_call {return_type; method_name; left_hand; args} ->
        let args = List.map expression_to_pholyglot args in
        Pholyglot_ast.Method_call {
            return_type = typ_to_pholyglot return_type;
            method_name;
            left_hand = expression_to_pholyglot left_hand;
            args;
        }
    | New (t, exprs) -> Pholyglot_ast.New (typ_to_pholyglot t, List.map expression_to_pholyglot exprs)
    | e -> failwith ("expression_to_pholyglot: " ^ (Ast.show_expression e))

let rec lvalue_to_pholyglot (l : Ast.lvalue) : Pholyglot_ast.lvalue = match l with
    | Ast.Variable id -> Pholyglot_ast.Variable id
    | Ast.Property_access class_property_name -> Pholyglot_ast.Property_access class_property_name
    | Ast.Object_access (identifier,  lvalue) -> Pholyglot_ast.Object_access (identifier, lvalue_to_pholyglot lvalue)

let statement_to_pholyglot s = match s with
    | Ast.Return exp -> Pholyglot_ast.Return (expression_to_pholyglot exp)
    | Ast.Assignment (typ, lvalue, expr) -> Pholyglot_ast.Assignment (typ_to_pholyglot typ, lvalue_to_pholyglot lvalue, expression_to_pholyglot expr)
    | Ast.Function_call (typ, identifier, exprs) -> Pholyglot_ast.Function_call (typ_to_pholyglot typ, identifier, List.map expression_to_pholyglot exprs)

let prop_to_pholyglot p : Pholyglot_ast.class_property = match p with
    | (name, t) -> (name, typ_to_pholyglot t)

let declaration_to_pholyglot (d : Ast.declaration) : Pholyglot_ast.declaration = match d with
    | Function {
        name;
        params;
        stmts;
        function_type;
    } ->
        Pholyglot_ast.Function {
            name;
            params = List.map param_to_pholyglot params;
            stmts = List.map statement_to_pholyglot stmts;
            function_type = typ_to_pholyglot function_type
        }
    | Class {name = class_name; kind = k; properties = props; methods} ->
        let fn : Ast.function_def -> Pholyglot_ast.function_def = fun {name; params; stmts; function_type;} ->
            {
                name;
                params = Param ("self", Class_type class_name) :: List.map param_to_pholyglot params;
                stmts = List.map statement_to_pholyglot stmts;
                function_type = typ_to_pholyglot function_type;
            }
        in
        Pholyglot_ast.Class (
            class_name,
            kind_to_pholyglot k,
            List.map prop_to_pholyglot props,
            List.map fn methods
        )

let declarations_to_pholyglot (ds : Ast.declaration list) : Pholyglot_ast.declaration list =
    List.map declaration_to_pholyglot ds 

(** Transpile from Pholly AST to Pholyglot AST *)
let run (ast : Ast.program) : Pholyglot_ast.program = match ast with
| Declaration_list ds -> 
    let declares : (Pholyglot_ast.declaration list) = List.map (fun d -> declaration_to_pholyglot d) ds in
    let open Pholyglot_ast in
    (
        Start_line,
        (* Include list *)
        [
            Include "stdio.h";
            Include "glib.h"
        ],
        (* C macros *)
        [
            Define ("class", Some "struct");
            Define ("__PHP__", Some "0");
            (* TODO: new_stack and new_heap? new_pool? Branch on _Generic: Point__stack, Point__boehm etc. *)
            Define ("new(x)", Some "x ## __constructor(alloca(sizeof(struct x)))");
            Define ("array(...)", Some "{__VA_ARGS__}");
            Define ("array_make(type, i, ...)", Some "{.thing = (type[]) array(__VA_ARGS__), .length = i}");
            Define ("array_get(type, arr, i)", Some "((type*) arr.thing)[i]");
            Define ("count(x)", Some "x.length");
            Define ("pprintf", Some "printf");
        ],
        (* C stubs *)
        [
            "typedef struct array array;\n";
            "struct array { void* thing; size_t length; };\n";
        ],
        (* PHP stubs *)
        [
            "class GString { public $str; public function __construct($str) { $this->str = $str; } }\n";
            "function g_string_new(string $str) { return new GString($str); }\n";
            "function g_string_append(GString $s1, string $s2) { return new GString($s1->str . $s2); }\n";
            (* The following constants are needed for the $type in array_get/array_make *)
            {|define("int", "int");|} ^ "\n";
            {|define("float", "float");|} ^ "\n";
            {|define("string", "string");|} ^ "\n";
            "function array_get($type, $arr, $i) { return $arr[$i]; }\n";
            "function array_make($type, $length, ...$values) { return $values; }\n";
            "function pprintf($format, ...$args) { fwrite( STDOUT, sprintf( $format, ...$args)); }\n";
        ],
        (* Declarations *)
        declares,
        End_line
    )

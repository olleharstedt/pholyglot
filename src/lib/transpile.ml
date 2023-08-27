open Printf

exception Transpile_error of string

let kind_to_pholyglot k = match k with
    | Ast.Ref -> Pholyglot_ast.Ref
    | Ast.Val -> Pholyglot_ast.Val

let alloc_to_pholyglot (a : Ast.allocation_strategy) : Pholyglot_ast.allocation_strategy = match a with
    | Stack -> Stack
    | Boehm -> Boehm
    | Arena -> Arena
    | Memory_context var -> Pholyglot_ast.Memory_context var
    | Memory_polymorph -> Pholyglot_ast.Memory_polymorph
    | Infer_allocation_strategy -> failwith "alloc_to_pholyglot: Must infer allocation strategy before transpile"

(** Transpile from Pholly AST to Pholyglot AST *)
let rec typ_to_pholyglot (t : Ast.typ) : Pholyglot_ast.typ = match t with
    | Int -> Pholyglot_ast.Int
    | Float -> Pholyglot_ast.Float
    | String -> Pholyglot_ast.String
    | String_literal -> Pholyglot_ast.String_literal
    | Constant -> Pholyglot_ast.Constant
    | Fixed_array (t, Some n) -> Pholyglot_ast.Fixed_array ((typ_to_pholyglot t), n)
    | Dynamic_array t -> Pholyglot_ast.Dynamic_array (typ_to_pholyglot t)
    | List t -> Pholyglot_ast.List (typ_to_pholyglot t)
    | Fixed_array (_, None) -> failwith "typ_to_pholyglot: array has size None, not fully inferred"
    | Void -> Pholyglot_ast.Void
    | Function_type {return_type; arguments} -> Pholyglot_ast.Function_type {return_type = typ_to_pholyglot return_type; arguments = List.map typ_to_pholyglot arguments}
    (** TODO: Should we infer types before moving to Pholyglot_ast? *)
    | Infer_me -> failwith "typ_to_pholyglot: Infer before transpiling"
    | Class_type (n, a) -> Pholyglot_ast.Class_type (n, alloc_to_pholyglot a)
    | t -> raise (Transpile_error ("typ_to_pholyglot: " ^ Ast.show_typ t))

let typ_to_pholyglot_constant (t : Ast.typ) : Pholyglot_ast.expression = match t with
    | Int -> Constant "int"
    | Float -> Constant "float"
    | String -> Constant "string"
    | Class_type (s, _) -> Constant s
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
    | Parenth e -> Pholyglot_ast.Parenth (expression_to_pholyglot e)
    | Plus (i, j) -> Pholyglot_ast.Plus (expression_to_pholyglot i, expression_to_pholyglot j)
    | Minus (i, j) -> Pholyglot_ast.Minus (expression_to_pholyglot i, expression_to_pholyglot j)
    | Times (i, j) -> Pholyglot_ast.Times (expression_to_pholyglot i, expression_to_pholyglot j)
    | Div (i, j) -> Pholyglot_ast.Div (expression_to_pholyglot i, expression_to_pholyglot j)
    | Concat (s, t) -> Pholyglot_ast.Concat (expression_to_pholyglot s, expression_to_pholyglot t)
    | Variable id -> Pholyglot_ast.Variable id
    | Lessthan (s, t) -> Pholyglot_ast.Lessthan (expression_to_pholyglot s, expression_to_pholyglot t)
    | Greaterthan (s, t) -> Pholyglot_ast.Greaterthan (expression_to_pholyglot s, expression_to_pholyglot t)
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
    (* TODO: Check Builtin_method_call here, to hide self variable *)
    | Method_call {return_type; method_name; left_hand; args} ->
        let args = List.map expression_to_pholyglot args in
        Pholyglot_ast.Method_call {
            return_type = typ_to_pholyglot return_type;
            method_name;
            left_hand = expression_to_pholyglot left_hand;
            args;
        }
    | New (None, _, _) as e -> failwith ("No inferred allocation_strategy: " ^ Ast.show_expression e)
    | New (Some alloc_strat, t, exprs) -> Pholyglot_ast.New (alloc_to_pholyglot alloc_strat, typ_to_pholyglot t, List.map expression_to_pholyglot exprs)
    | List_init t -> Pholyglot_ast.List_init (typ_to_pholyglot t)
    | e -> failwith ("expression_to_pholyglot: " ^ (Ast.show_expression e))

let rec lvalue_to_pholyglot (l : Ast.lvalue) : Pholyglot_ast.lvalue = match l with
    | Ast.Variable id -> Pholyglot_ast.Variable id
    | Ast.Property_access class_property_name -> Pholyglot_ast.Property_access class_property_name
    | Ast.Object_access (identifier,  lvalue) -> Pholyglot_ast.Object_access (identifier, lvalue_to_pholyglot lvalue)

let rec statement_to_pholyglot s = match s with
    | Ast.Return exp -> Pholyglot_ast.Return (expression_to_pholyglot exp)
    | Ast.Plusplus v -> Pholyglot_ast.Plusplus (lvalue_to_pholyglot v)
    | Ast.Minusminus v -> Pholyglot_ast.Minusminus (lvalue_to_pholyglot v)
    | Ast.Pluseq (v, e) -> Pholyglot_ast.Pluseq (lvalue_to_pholyglot v, expression_to_pholyglot e)
    | Ast.Minuseq (v, e) -> Pholyglot_ast.Minuseq (lvalue_to_pholyglot v, expression_to_pholyglot e)
    | Ast.Assignment (typ, lvalue, expr) -> Pholyglot_ast.Assignment (typ_to_pholyglot typ, lvalue_to_pholyglot lvalue, expression_to_pholyglot expr)
    | Ast.Method_call {lvalue; args} -> Pholyglot_ast.Method_call {lvalue = lvalue_to_pholyglot lvalue; args = List.map expression_to_pholyglot args}
    | Ast.Function_call (typ, identifier, exprs) -> Pholyglot_ast.Function_call (typ_to_pholyglot typ, identifier, List.map expression_to_pholyglot exprs)
    (* TODO: foreach ([1, 2, 3] as $i) { ... } *)
    | Ast.Foreach {arr = Variable arr_; key; value = Variable value_var; value_typ; value_typ_constant; body;} ->
        let key_name = match key with Some (Variable s) -> s | None -> "__i" in
        Pholyglot_ast.For {
            (* TODO: Generate 'i' variable *)
            init      = Pholyglot_ast.Assignment (Int, Variable key_name, Num 0);
            condition = Pholyglot_ast.Lessthan (
                Variable key_name,
                Function_call (
                        Function_type {return_type = Int;
                        arguments = [Fixed_array (Int, 0)]
                    },
                    "count",
                    [expression_to_pholyglot (Variable arr_)]
                )
            );
            incr      = Equal (Variable key_name, Plus (Variable key_name, Num 1));
            stmts     =
                Pholyglot_ast.Assignment (
                    typ_to_pholyglot value_typ,
                    Variable value_var,
                    Function_call (
                        Function_type {return_type = typ_to_pholyglot value_typ; arguments = [Fixed_array (Int, 0)]},
                        "array_get",
                        [expression_to_pholyglot value_typ_constant; expression_to_pholyglot (Variable arr_); Variable key_name]
                    )
                )
                :: List.map statement_to_pholyglot body;
        }
    | Ast.Foreach_list {arr = Variable arr_; key; value = Variable value_var; value_typ; body;} ->
        let value_typ = typ_to_pholyglot value_typ in
        Pholyglot_ast.Dowhile {
            before = Some (
                Lib_method_call {
                    function_name = "rewind";
                    variable_name = arr_;
                }
            );
            condition = Lib_method_call {
                function_name = "valid";
                variable_name = arr_;
            };
            body =
                Assignment (
                    value_typ,
                    Variable value_var,
                    Lib_method_call {
                        function_name = "current";
                        variable_name = arr_;
                    };
                )
            :: (List.map statement_to_pholyglot body @
                [Lib_method_call {
                    function_name = "next";
                    variable_name = arr_
                }]
            )
        }
(*
    $list->rewind(
        #__C__ $list
    );
    do {
        #__C__ Point
        $tmp = $list->current(
            #__C__ $list
        );
        if ($tmp) {
            printf("Current point x = %ld\n", $tmp->x);
        } else {
            printf("No current :(\n");
        }   
        $list->next(
            #__C__ $list
        );
    } while ($list->valid(
        #__C__ $list
    ));
*)
    | Dowhile {condition; body;} -> Pholyglot_ast.Dowhile {
        before    = None;
        condition = expression_to_pholyglot condition;
        body      = List.map statement_to_pholyglot body;
    }

let prop_to_pholyglot p : Pholyglot_ast.class_property = match p with
    | (name, t) -> (name, typ_to_pholyglot t)

(*
 * TODO: Done too late in pipeline? At string echo
let replace_this_with_self (stmt : Pholyglot_ast.statement ) : Pholyglot_ast.statement = match stmt with
    | Return e
    | Plusplus lvalue
    | Minusminus lvalue
    | Minuseq (lvalue * expression
    | Pluseq of lvalue * expression
    | Assignment of typ * lvalue * expression
    | Function_call of typ * identifier * expression list
    | For of {
        init:      statement;       (* Init happens outside the for-statement *)
        condition: expression;
        incr:      expression;
        stmts:     statement list;
    }
    | Dowhile of {
        condition: expression;
        body:      statement list;
    }
*)


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
                (* TODO: Alloc strat should be None here? Or Irrelevant? *)
                params = Param ("__self", Class_type (class_name, Boehm)) :: List.map param_to_pholyglot params;
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
            Include "stdlib.h";
            Include "stdint.h";
            Include "glib.h";
            Include "math.h";
            Include "phollylib.c";
        ],
        (* C macros *)
        [
            (* Moved to phollylib *)
        ],
        (* C stubs *)
        [
            (* Moved to phollylib *)
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
            {|define("SplDoublyLinkedList", "SplDoublyLinkedList");|} ^ "\n";
            "function array_get($type, $arr, $i) { return $arr[$i]; }\n";
            "function array_make($type, $length, ...$values) { return $values; }\n";
            "function pprintf($format, ...$args) { fwrite( STDOUT, sprintf( $format, ...$args)); }\n";
        ],
        (* Declarations *)
        declares,
        End_line
    )

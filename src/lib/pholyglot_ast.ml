(* This is the AST for the PHP+C polyglot code *)

open Printf
open String
open Ppx_compare_lib.Builtin
open Sexplib.Std

type program = 
    start_line * 
    includes list * 
    define list * 
    string list * (* C stubs *)
    php_stubs list * 
    declaration list * 
    end_line
[@@deriving show, compare, sexp]

and start_line = Start_line
and end_line = End_line

and kind =
    | Ref
    | Val

and allocation_strategy =
    | Boehm
    | Stack
    | Arena
    | Heap
    (* TODO: Not 100% sure here, will only work for input arguments? *)
    | Memory_polymorph
    | Memory_context of string

and typ =
    | Int
    | Float
    | String                (* Actually GString pointer *)
    | String_literal
    | String_gstring
    | Constant
    | Void
    | Var_args
    | Fixed_array of typ * int
    | Dynamic_array of typ
    | List of typ
    | Function_type of {
		return_type: typ;
        (* TODO: Not needed in Pholyglot_ast? *)
        arguments: typ list
    }
    | Class_type of class_name * allocation_strategy
    | Hash_table of typ * typ     (* Key type, value type *)

and param =
    | Param of identifier * typ
    | RefParam of identifier * typ

and docblock_comment =
    | DocParam of identifier * typ

and function_def = {
        name:           function_name;
        params:         param list;
        stmts:          statement list;
        function_type:  typ
    }

and declaration =
    | Function of function_def
    | Class of class_name * kind * class_property list * function_def list

and function_name = string
and class_name = string
and identifier = string
and include_lib = string
and php_stubs = string
and class_property_name = string
and class_property = class_property_name * typ

and statement =
    | Return of expression
    | Plusplus of lvalue
    | Minusminus of lvalue
    (** TODO: Not needed if you use assignment instead? *)
    | Minuseq of lvalue * expression
    | Pluseq of lvalue * expression
    | Assignment of typ * lvalue * expression
    | Function_call of typ * identifier * expression list
    | Method_call of {
        lvalue:  lvalue;
        args:    expression list;
        builtin: bool;
    }
    | Lib_method_call of {
        lvalue:  lvalue;
        args:    expression list;
        builtin: bool;
    }
    | For of {
        init:      statement;       (* Init happens outside the for-statement *)
        condition: expression;
        incr:      expression;
        stmts:     statement list;
    }
    | Dowhile of {
        before:    expression option; (* To traverse SplDoublyLinkedList you need to call reset before the loop. *)
        condition: expression;
        body:      statement list;
    }
    | Hash_set of {
        hash_var: lvalue;   (* $ht *)
        hash_typ: typ;      (* Hash_table (key, value) *)
        key: expression;    (* 10 in $ht[10] *)
        value: expression;  (* $ht[10] = <value> *)
        (* No value_t? Assuming value_t = Hash_table value def? *)
    }
    | C_only of statement (* Used for #__C__ GC_INIT() etc *)
    | C_only_string of string

and lvalue =
    | Variable of identifier
    | Property_access of class_property_name
    | Object_access of identifier * lvalue

and expression =
    | Num of int
    | Num_float of float
    | String of string
    | Constant of string (* Used in array_make(int, 2, 1, 2) *)
    | Parenth of expression
    | Plus of expression * expression
    | Minus of expression * expression
    | Times of expression * expression
    | Div of expression * expression
    | Concat of expression * expression
    | Lessthan of expression * expression
    | Greaterthan of expression * expression
    | Equal of expression * expression
    | Variable of identifier
    | Array_init of expression list
    | Hash_init of typ
    | List_init of typ (* SplDoublyLinkedList *)
    | Array_access of identifier * expression
    | Object_access of expression * expression
    | New of allocation_strategy * typ * expression list
    | Clone of {
        variable_name: string;
        t:             typ;
        alloc_strat:   allocation_strategy
    }
    | Property_access of identifier (* Valid sub-expression of object access *)
    | Method_call of {
        return_type: typ;
        method_name: string;
        left_hand:   expression;
        args:        expression list;
        builtin:     bool;
    }
    | Builtin_method_call of {
        return_type: typ;
        method_name: string;
        left_hand: expression;
        args: expression list;
    }
    | Function_call of typ * identifier * expression list
    | Coerce of typ * expression
    | Self
    (* Lib object calls must include the self in Pholyglot, so function signature will be different *)
    (* TODO: Might need to extend this with function arguments. *)
    | Lib_method_call of {
        lvalue:  lvalue;
        args:    expression list;
        builtin: bool;
    }

and includes =
    | Include of include_lib

and define =
    | Define of identifier * string option

let string_of_start_line = function Start_line -> {|//<?php echo "\x08\x08"; ob_start(); ?>
|}


let string_of_include = function
    | Include l -> sprintf "#include <%s>\n" l

let string_of_define (d : define) : string = match d with
    | Define (id, Some s) -> sprintf "#define %s %s\n" id s
    | Define (id, None) -> sprintf "#define %s \n" id

let rec string_of_typ (t : typ) : string = match t with
    (* We need to use long and double here to keep same size as uintptr_t *)
    | Int -> "int"
    | Float -> "float"
    | String -> "GString*"
    | Void -> "void"
    | Fixed_array (t, n) -> (*string_of_typ t*) "array"
    | Dynamic_array t -> (* string_of_typ t *) "array"
    | List t -> "SplDoublyLinkedList"
    (* Assuming we have a proper typedef, this is OK in both PHP and C *)
    | Class_type (n, a) -> n
    | Function_type {return_type; arguments} -> string_of_typ return_type
    | Hash_table (k, v) -> "ArrayObject"

(** Type notation that goes AFTER the variable name, as in array init *)
(* TODO: No longer needed with array struct {.thing void*, .length int} *)
let string_of_typ_post = function
    | Fixed_array (t, n) -> sprintf {||}
    | _ -> ""

let string_of_param (p : param) : string = match p with
    | Param (id, t) -> string_of_typ t ^ " $" ^ id
    | RefParam (id, t) -> string_of_typ t ^ " &$" ^ id

(**
 * Needed because of '&' and array value semantics in PHP.
 *)
let string_of_param_without_ref (p : param) : string = match p with
    | Param (id, t)
    | RefParam (id, t) -> string_of_typ t ^ " $" ^ id

let string_of_alloc_strat strat = match strat with
    | Boehm -> "gc_mem"
    | Arena -> "arena_mem"
    | Heap  -> "heap_mem"
    | Memory_polymorph -> failwith "string_of_alloc_strat: Cannot stringify Memory_polymorph"
    | Memory_context s -> "$" ^ s ^ "->mem"
    | _ -> failwith "string_of_alloc_strat: Unknown allocation strat"

let rec string_of_lvalue (l : lvalue) : string = match l with
    | Variable id -> id
    | Object_access (id, Property_access prop_name) ->
        (* TODO Code duplication *)
        let id = if id = "this" then "__self" else id in
        sprintf {|%s->%s|} id prop_name
    | Property_access n -> n

let rec string_of_expression : expression -> string = function
    | Num i -> Int.to_string i
    | Num_float f -> Float.to_string f
	(* TODO: Problem with GString vs string for expressions, append vs use as function arg *)
    | String s -> sprintf "g_string_new(%s)" s
    | Constant s -> s
    | Parenth e -> "(" ^ string_of_expression e ^ ")"
    | Coerce (String_literal, String s) -> s
    | Coerce (String_gstring, Variable s) -> "$" ^ s ^ "->str"
    | Coerce (t, expr) -> failwith (
        sprintf
        "string_of_expression: Coerce: Expected String_literal String s, but got %s %s"
        (show_typ t)
        (show_expression expr)
    )
    | Plus (i, j) -> (string_of_expression i) ^ " + " ^ (string_of_expression j)
    | Minus (i, j) -> (string_of_expression i) ^ " - " ^ (string_of_expression j)
    | Times (i, j) -> (string_of_expression i) ^ " * " ^ (string_of_expression j)
    | Div (i, j) -> (string_of_expression i) ^ " / " ^ (string_of_expression j)
    | Concat (s, t) -> sprintf "g_string_append(%s, %s->str)" (string_of_expression s) (string_of_expression t)
    | Lessthan (s, t) -> sprintf "%s < %s" (string_of_expression s) (string_of_expression t)
    | Greaterthan (s, t) -> sprintf "%s > %s" (string_of_expression s) (string_of_expression t)
    | Equal (s, t) -> sprintf "%s = %s" (string_of_expression s) (string_of_expression t)
    | Variable id ->
        let id = if id = "this" then "__self" else id in
        "$" ^ id
        (*
"string_of_expression: (Pholyglot_ast.New (Pholyglot_ast.Arena,\
   (Pholyglot_ast.List\
      (Pholyglot_ast.Class_type (\"Point\", Pholyglot_ast.Boehm))),\
   [(Pholyglot_ast.List_init\
       (Pholyglot_ast.List\
          (Pholyglot_ast.Class_type (\"Point\", Pholyglot_ast.Boehm))))\
     ]\
   ))").
         *)
    | New (alloc_strat, List (Class_type (name, _)), exprs) -> begin
        let mem_f = string_of_alloc_strat alloc_strat in
        [%string {|new(SplDoublyLinkedList
#__C__, $mem_f
)|}]
    end
    | New (alloc_strat, Class_type (ct, c_alloc_strat), exprs) -> begin
        if not (Caml.(=) alloc_strat c_alloc_strat) then failwith "alloc_strat must equal c_alloc_strat";
        let mem_f = string_of_alloc_strat alloc_strat in
        [%string {|new($ct
#__C__, $mem_f
)|}]
    end
    | New (alloc_strat, Hash_table (k, v), exprs) -> begin
        let mem_f = string_of_alloc_strat alloc_strat in
        [%string {|new(ArrayObject
#__C__, $mem_f
)|}]
    end
    | Clone {variable_name; t; alloc_strat} ->
        let t_s = string_of_typ t in
        let mem_f = string_of_alloc_strat alloc_strat in
        [%string {|clone($$$variable_name
#__C__, $t_s, $mem_f
)|}]
    | Array_init exprs -> sprintf {|array_make(%s)|}
        (Base.String.concat ~sep:", " (Base.List.map exprs ~f:string_of_expression))
    | Array_access (id, expr) ->
        (* TODO: It's assumed that expr has type Int here *)
        sprintf {|$%s[%s]|} id (string_of_expression expr)
    | Object_access (Variable id, Property_access prop_name) ->
        (* TODO: Code duplication *)
        let id = if id = "this" then "__self" else id in
        sprintf {|$%s->%s|} id prop_name
    | Object_access (expr, Property_access prop_name) ->
        sprintf {|%s->%s|} (string_of_expression expr) prop_name
    | Method_call {return_type; method_name; left_hand = Variable object_name; args} ->
        let args = (Variable object_name : expression ) :: args in
        sprintf {|$%s->%s(%s)|}
        object_name
        method_name 
        (Base.String.concat ~sep:", " (Base.List.map args ~f:string_of_expression))
    | Property_access n -> n
    | Function_call (Function_type {return_type; arguments}, name, param_exprs) ->
        sprintf
        {|%s(%s)|}
        name
        (Base.String.concat ~sep:", " (Base.List.map param_exprs ~f:string_of_expression))
    | Lib_method_call {lvalue = Object_access (id, Property_access m); args} ->
        if List.length args > 0 then begin
            let args = Base.String.concat ~sep:", " (Base.List.map args ~f:string_of_expression) in
            [%string {|$$$id->$m(
#__C__ $$$id,
$args
)|}]
        end else
            [%string {|$$$id->$m(
#__C__ $$$id
)|}]
    (*| Method_call {return_type; method_name; object_name; args} ->*)
    | e -> failwith ("string_of_expression: " ^ show_expression e)

let rec string_of_statement = function
    | Return exp -> "return " ^ string_of_expression exp ^ ";\n"
    | Plusplus (Variable v) -> string_of_expression (Variable v) ^ "++;\n"
    | Minusminus (Variable v) -> string_of_expression (Variable v) ^ "--;\n"
    | Pluseq (Variable v, e) -> string_of_expression (Variable v) ^ " += " ^ string_of_expression e ^ ";\n"
    | Minuseq (Variable v, e) -> string_of_expression (Variable v) ^ " -= " ^ string_of_expression e ^ ";\n"
    | Function_call (Function_type {return_type = Void; arguments}, id, exprs) ->
        sprintf {| %s(%s);
    |}
        id
        (Base.String.concat ~sep:", " (Base.List.map exprs ~f:string_of_expression))
    | Function_call (fun_type, id, _) ->
        failwith (
            sprintf
            "string_of_statement: Function_call: Can only call functions that return void as statements; %s does not: %s"
            id
            (string_of_typ fun_type)
        )
    | Method_call {lvalue = Object_access (id, Property_access m); args} ->
        (** TODO: Ugle hack to remove __prop_ - shouldn't be here in the first place *)
        let m = Str.global_replace (Str.regexp "__prop_") "" m in
        let args : expression list = Variable id :: args in
        let args_s   = Base.String.concat ~sep:", " (Base.List.map args ~f:string_of_expression) in
        [%string {|$$$id->$m($args_s);
|}]
    | Lib_method_call {lvalue = Object_access (id, Property_access m); args} ->
        (** TODO: Ugle hack to remove __prop_ - shouldn't be here in the first place *)
        let m = Str.global_replace (Str.regexp "__prop_") "" m in
        let args_s   = Base.String.concat ~sep:", " (Base.List.map args ~f:string_of_expression) in
        if List.length args > 0 then
        [%string {|$$$id->$m(
    #__C__ $$$id,
    $args_s);
|}]
        else
        [%string {|$$$id->$m(
    #__C__ $$$id
    );
|}]
    | Assignment (typ, Variable v, expr) -> sprintf {|#__C__ %s
    $%s %s= %s;
    |}
    (string_of_typ typ)
    v
    (string_of_typ_post typ)
    (string_of_expression expr)
    | Assignment (typ, lvalue, expr) -> sprintf {|$%s %s= %s;
    |}
    (string_of_lvalue lvalue)
    (string_of_typ_post typ)
    (string_of_expression expr)
    | For {init; condition; incr; stmts;} ->
        let init_s = string_of_statement init in
        let stmts_s = Base.String.concat (Base.List.map stmts ~f:string_of_statement) in
        let condition_s = string_of_expression condition in
        let incr_s = string_of_expression incr in
        [%string {|$init_s
        for (; $condition_s; $incr_s) {
            $stmts_s
        }
    |}]
    | Dowhile {condition; body; before;} ->
        let before_expr = match before with Some b -> string_of_expression b  ^ ";" | None -> "" in
        let condition_s = string_of_expression condition in
        let body_s      = Base.String.concat (Base.List.map body ~f:string_of_statement) in
        [%string {|$before_expr
do {
$body_s} while ($condition_s);
|}]
    | C_only_string s -> s
    | Hash_set {hash_var; hash_typ; key; value;} ->
        let Variable id = hash_var in
        let key = string_of_expression key in
        let value = string_of_expression value in
        [%string {|$$$id->offsetSet(
    #__C__ $$$id,
    $key, $value);
|}]
    | s -> failwith ("Pholyglot_ast.string_of_statement: Unsupported statement: " ^ show_statement s)

let string_of_prop (p : class_property) : string = match p with
    | (n, t) -> sprintf {|#define public %s
#define %s $%s
    public $%s;
#undef public
|}
    (string_of_typ t)
    n
    n
    n

(* Function pointers are defined BEFORE the typedef, so 'struct' needs to be written explicitly for class types *)
(* char* (*getName) (struct Point*); *)
let string_of_function_pointer meth : string = match meth with
  (*| (name, params, stmts, Function_type {return_type; arguments}) ->*)
    | { name; params; stmts; function_type = Function_type {return_type; arguments}} ->
    sprintf {|#__C__ %s (*%s) (%s); |}
    (* Return type *)
    (string_of_typ (Function_type {return_type; arguments}))
    (* Function name *)
    name
    (* Params *)
    (Base.String.concat ~sep:", " (Base.List.map params ~f:string_of_param))

(**
 * Returns true if any param is a RefParam
 * Reason is that we can't hide the PHP ampersand and this notation is by asterisk pointer in C.
 * So this function decides if we need to split function signature in two or not.
 *)
let params_has_ref (params : param list ) : bool =
    let refparams = Base.List.filter params ~f:(fun p -> match p with RefParam _ -> true | Param _ -> false) in
    match refparams with [] -> true | _ -> false

(**
 * A method must be valid both as C function and PHP class method
 *
 * @param name : string Class name, used by self
 * @param meth : method
 * @return string
 *)
let string_of_method class_name meth = match meth with
    | {name = method_name; params; stmts; function_type = Function_type {return_type; arguments}} ->
        let return_type_s = string_of_typ (Function_type {return_type; arguments}) in
        let params_s = Base.String.concat ~sep:", " (Base.List.map params ~f:string_of_param) in
        let stmts_s  = (Base.String.concat (Base.List.map stmts ~f:string_of_statement)) in
        [%string {|
#__C__ $return_type_s $(class_name)__$(method_name) ($params_s)
#if __PHP__
public function $method_name($params_s): $return_type_s
#endif
{
    $stmts_s
}
|}]

let string_of_function_pointer_init class_name meth = match meth with
    | { name; } ->
        sprintf {|$p->%s = &%s__%s;
|}
        name
        class_name
        name

let string_of_declare (d : declaration) : string = match d with
    | Function {
        name = function_name;
        params;
        stmts;
        function_type = typ
    } ->
        let stmts = if function_name = "main" then C_only_string "#__C__ GC_INIT();\n" :: stmts else stmts in
        (* TODO: Find out if stmts contain any arena alloc *)
        let typ_s = string_of_typ typ in
        let params_s = Base.String.concat ~sep:", " (Base.List.map params ~f:string_of_param) in
        let stmts_s = (Base.String.concat (Base.List.map stmts ~f:string_of_statement)) in
        if params_has_ref params then begin
            [%string {|#define function $typ_s
function $function_name($params_s)
#undef function
{
    $stmts_s}
|}]
        end else begin
            let params_without_ref_s = Base.String.concat ~sep:", " (Base.List.map params ~f:string_of_param_without_ref) in
            [%string {|#__C__ $typ_s $function_name($params_without_ref_s)
#if __PHP__
function $function_name($params_s): $typ_s
#endif
{
    $stmts_s}
|}]
        end
    | Class (class_name, kind, props, methods) ->
        let string_of_method = fun (m) -> string_of_method class_name m in
        let string_of_function_pointer_init = fun (m) -> string_of_function_pointer_init class_name m in
        let props = (Base.String.concat (Base.List.map ~f:string_of_prop props)) in
        let function_pointers = (Base.String.concat (Base.List.map ~f:string_of_function_pointer methods)) in
        let function_pointers_init = (Base.String.concat (Base.List.map ~f:string_of_function_pointer_init methods)) in
        let methods = (Base.String.concat (Base.List.map ~f:string_of_method methods)) in
        (* TODO: Fix macro for sizeof *)
        [%string {|
#__C__ typedef struct $class_name* $class_name;
class $class_name {
    $props
    #__C__ struct mem mem;
    $function_pointers
// End of C struct def. Class methods are outside the struct.
#__C__ };
$methods
#if __PHP__
// End of PHP class def.
};
#endif
#if __PHP__
define("$class_name", "$class_name");
#endif
//?>
// Function pointer init
$class_name $(class_name)__constructor($class_name $$p, struct mem m)
{
    $function_pointers_init
    $$p->mem = m;
    return $$p;
}
$class_name $(class_name)__clone($class_name $$self, struct mem m)
{
    $class_name clone = ($class_name) m.alloc(m.arena, sizeof (struct $class_name));
    memcpy(clone, $$self, sizeof (struct $class_name));
    return clone;
}
//<?php
|}]

(** Probably only to be used in tests *)
let string_of_declares ds : string = Base.String.concat (Base.List.map ds ~f:string_of_declare)

let string_of_end_line = function End_line -> {|// ?>
// <?php ob_end_clean(); main();|}

let string_of_program (p : program) : string = match p with
    | (s, is, ds, c_stubs, php_stubs, decs, e) ->
        Base.String.concat
        [
            string_of_start_line s;
            Base.String.concat (Base.List.map is ~f:string_of_include);
            Base.String.concat (Base.List.map ds ~f:string_of_define);
            Base.String.concat c_stubs;
            "#if __PHP__//<?php\n";
            Base.String.concat php_stubs;
            "#endif//?>\n";
            "//<?php\n";
            Base.String.concat (Base.List.map decs ~f:string_of_declare);
            string_of_end_line e
        ]

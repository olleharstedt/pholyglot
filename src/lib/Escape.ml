open Ast
open Printf

type escape_status = Escapes | Safe
[@@deriving show, compare, sexp]

(* Simple assoc list as graph *)
(* var points to var1, var2, ... *)
type alias_graph = (identifier, identifier) Hashtbl.t

type escapes_data = (identifier, escape_status) Hashtbl.t

(* TODO: Replace with alloc type: heap, stack, pool *)
let allowed_to_escape : (typ -> bool) = function
    | Int -> true
    | _ -> false


let run : (program -> escape_status list) = function
    | Declaration_list decls ->
        (* Loop through all functions and statements to create namespace bindings *)
        let iter_decl = function
            | Function (name, params, stmts, typ) as fn ->
                let namespace = Namespace.create () in
                Namespace.add_assignments namespace fn;
                Escapes
        in
        List.map iter_decl decls
        (* Create a graph over what variables points to what, aliasing *)
        (* Start with things you know escapes *)
        (* If nothing escapes, you're safe *)
        (* If some things escape, check if they're safe to escape *)
        (* Throw error if they're not safe *)

        (* TODO: What points to heap? Does it matter? Or other way around, if it does not escape, don't use heap. *)
        (* TODO: It's OK to point to heap if the variable lives forever *)

(* Get all variables that are returned from function f, or contained in data structures returned from f *)
(* Some primitive types are allowed to escape, if they're copied on return, like int or int array *)
let get_basic_escapes_data (namespace : Namespace.t) (f : declaration) : escapes_data =
    let ed : escapes_data = Hashtbl.create 10 in
    (* Help function *)
    let rec iter_stmts : (statement list -> unit) = function
        (* TODO: Return (New typ, exprs) *)
        | [] -> ()
        | Return (Variable id) :: ss -> begin
            begin match Namespace.find namespace id with
            | Some typ -> if  not (allowed_to_escape typ) then Hashtbl.add ed id Escapes
            | None -> failwith (sprintf "get_basic_escapes_data: Did not find the typ of variable %s in namespace" id)
            end;
            iter_stmts ss
        end
        | Return expr :: ss -> begin
            let t = Infer.typ_of_expression expr in
            if allowed_to_escape t then
                iter_stmts ss
            else
                failwith "get_basic_escapes_data: expression type is not allowed to escape: " (show_typ t)
        end
        | Function_call (_, _, _) :: ss -> iter_stmts ss
        | Assignment _ :: ss -> iter_stmts ss
    in
    match f with
    | Function (name, params, stmts, typ) as fn -> 
        Namespace.add_assignments namespace fn;
        iter_stmts stmts;
        ed
    | _ -> failwith "get_basic_escapes_data: Can only get escape data from function declaration"
    (* TODO: Method escape data *)

let get_alias_graph (namespace : Namespace.t) (fun_decl : declaration) : alias_graph =
    let aliases : alias_graph = Hashtbl.create 10 in
    let rec iter_stmts : (statement list -> unit) = function
        | [] -> ()
        (* TODO: All lvalues *)
        | Assignment (typ, Identifier id, Variable id2 ) :: tail ->
            Hashtbl.add aliases id id2;
            iter_stmts tail
        | Assignment (typ, id, expr) :: tail ->
            (* TODO: Check type of expr? *)
            iter_stmts tail
        | s :: ss -> iter_stmts ss
    in
    match fun_decl with
    | Function (name, params, stmts, typ) as f ->
        let _ = Namespace.add_assignments namespace f in
        iter_stmts stmts;
        aliases

(* TODO: Combine basic escape data with connection graph to figure out what escapes *)

let%test_unit "trivial string escape" =
    let source = {|<?php // @pholyglot
    function main(): int {
        $a = "moo";
        return $a;
    }
    |} in
    let Declaration_list [fn] =
        Lexing.from_string source |>
        Parser.program Lexer.token
    in
    let namespace = Namespace.create () in
    let ed = get_basic_escapes_data namespace fn in
    let l = Hashtbl.to_seq ed |> List.of_seq in
    let open Base in
    [%test_eq: (string * escape_status) list] [("a", Escapes)] l

let%test_unit "trivial no string escape" =
    let source = {|<?php // @pholyglot
    function main(): int {
        $a = "moo";
        printf("%s", $a);
        return 0;
    }
    |} in
    let Declaration_list [fn] =
        Lexing.from_string source |>
        Parser.program Lexer.token
    in
    let namespace = Namespace.create () in
    let ed = get_basic_escapes_data namespace fn in
    let l = Hashtbl.to_seq ed |> List.of_seq in
    let open Base in
    [%test_eq: (string * escape_status) list] [] l


let%test_unit "trivial alias graph" =
    let source = {|<?php // @pholyglot
    function main(): int {
        $a = "moo";
        $b = $a;
        $c = $a;
        return $b;
    }
    |} in
    let Declaration_list [fn] =
        Lexing.from_string source |>
        Parser.program Lexer.token
    in
    let namespace = Namespace.create () in
    let aliases = get_alias_graph namespace fn in
    let l = Hashtbl.to_seq aliases |> List.of_seq in
    let open Base in
    [%test_eq: (string * string) list] [("b", "a"); ("c", "a")] l

(* $a = moo; return $a . "foo"; *)

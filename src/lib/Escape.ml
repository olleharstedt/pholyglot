open Ast
open Printf

type escape_status = Escapes | Safe
[@@deriving show, compare, sexp]

(* Simple assoc list as graph *)
(* var points to var1, var2, ... *)
type connection_graph = (identifier, identifier list) Hashtbl.t

type escapes_data = (identifier, escape_status) Hashtbl.t

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
        | Assignment _ :: ss -> iter_stmts ss
    in
    match f with
    | Function (name, params, stmts, typ) as fn -> 
        Namespace.add_assignments namespace fn;
        iter_stmts stmts;
        ed
    | _ -> failwith "get_basic_escapes_data: Can only get escape data from function declaration"
    (* TODO: Method escape data *)

(*
let get_connection_graph fun_decl namespace =
    let graph : connection_graph = Hashtbl.create 10 in
    match fun_decl with
    | Function (name, params, stmts, typ) as f ->
        let _ = Namespace.add_assignments namespace f in
        (* Every Variables inside a Return statements escapes? Not return $a + $b; But return [$a]; *)
        let ed = get_basic_escapes_data namespace f in
        ()
*)

let%test_unit "trivial string escape" =
    let source = {|<?php // @pholyglot
    function main(): int {
        $a = "Moo";
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

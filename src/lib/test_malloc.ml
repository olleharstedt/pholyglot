open Printf
module Log = Dolog.Log

(**
 * Malloc as an alloc type is needed to deal with third-party library functions like file_get_contents.
 *)

let%test_unit "WIP" =
    let source = {|<?php // @pholyglot
class Point {}
function foo(): void
{
    $p = /** @alloc heap */ new Point();
}
|}
    in
    let linebuf = Lexing.from_string source in
    let ns = Namespace.create () in
    let ast = Parser.program Lexer.token linebuf |> Infer.run ns in
    [%test_eq: Ast.program] (Declaration_list []) (Declaration_list [])

(* TODO
 * malloc is not allowed to escape
 *   shortcut without escape analysis? Not allowed on right-hand side without a mem clone/copy
 *)

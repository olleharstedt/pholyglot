open Pholyglot
open Printf

exception Parser_error of string
exception Lexer_error of string
exception Internal_error of string
let _ =
    (* TODO: Read from file *)
    (*
    let source = {|<?php // @pholyglot
    function main(): int {
        $str = "Hello" . " world" . "!";
        return 0;
    } |} in
    *)

	let read_whole_file filename =
		let ch = open_in filename in
		let s = really_input_string ch (in_channel_length ch) in
		close_in ch;
		s
    in

    let source = read_whole_file (Sys.argv.(1)) in

    (* NAME int NAME main LPAREN RPAREN LBRACE RETURN INT0 SEMICOLON RBRACE *)
    let linebuf = Lexing.from_string source in

    let ast = try (Parser.program Lexer.token linebuf) with
      | Lexer.Error msg ->
          let tok : string = Lexing.lexeme linebuf in
          raise (Lexer_error (sprintf "%s token = %s" msg tok))
      | Parser.Error ->
          let tok = Lexing.lexeme linebuf in
          raise (Parser_error (sprintf "tok = %s, Could not parse '%s': error at %c" tok source (String.get source (Lexing.lexeme_start linebuf))))
          (*raise (Parser_error (sprintf "Could not parse '%s'" source ))*)
      | Failure msg ->
          let open Lexing in
          print_endline msg;
          raise (Internal_error (sprintf "line = %d; col = %d" linebuf.lex_curr_p.pos_lnum linebuf.lex_curr_p.pos_cnum))
    in
    let ns = Namespace.create () in
    let ast = Pholyglot.Infer.run ns ast in
    let phast = Pholyglot.Transpile.run ast in
    let pholly = Pholyglot.Pholyglot_ast.string_of_program phast in


open Pholyglot
open Printf

exception Parser_error of string
exception Lexer_error of string
exception Internal_error of string

let string_of_token (token : Parser.token) : string =
    let open Parser in
    match token with
        | WITH -> "WITH"
        | STRUCT -> "STRUCT"
        | STRING_LITERAL -> "STRING_LITERAL"
        | SEMICOLON -> "SEMICOLON"
        | RPAREN -> "RPAREN"
        | RETURN -> "RETURN"
        | RBRACK -> "RBRACE"
        | RBRACE -> "RBRACE"
        | PLUS -> "PLUS"
        | NEW -> "NEW"
        | NAME s  -> "NAME " ^ s
        | MINUS -> "MINUS"
        | LT -> "LT"
        | LPAREN -> "LPAREN"
        | LET -> "LET"
        | LBRACK -> "LBRACK"
        | LBRACE -> "LBRACE"
        | INT i -> "INT" ^ string_of_int i
        | GT -> "GT"
        | EQEQ -> "EQEQ"
        | EQ -> "EQ"
        | EOF -> "EOF"
        | CONSTANT -> "CONSTANT"
        | START_SCRIPT -> "START_SCRIPT"
        | FUNCTION -> "FUNCTION"
        | _ -> failwith "Unknown token"

let _ =
    let source = "<?php // @pholyglot
    function main(): int {
        $a = 0;
        return $a + 1 - 1 * 1 / 2;
    }
    " in

    (* NAME int NAME main LPAREN RPAREN LBRACE RETURN INT0 SEMICOLON RBRACE *)
    let linebuf = Lexing.from_string source in

    (*
    let rec dump_tokens linebuf =
        let token = Lexer.token linebuf in
        match token with
            | Parser.EOF -> ()
            | t ->
                printf "%s" ((string_of_token t) ^ " ");
                dump_tokens linebuf
    in
    dump_tokens linebuf;
    *)

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
    let ast = Pholyglot.Infer.run ast in
    let phast = Pholyglot.Transpile.run ast in
    print_endline (Pholyglot.Pholyglot_ast.string_of_program phast)

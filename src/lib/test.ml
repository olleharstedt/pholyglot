open Base

let%test_unit "trivial" =
    let source = "<?php // @pholyglot" in
    let linebuf = Lexing.from_string source in
    let ast = Parser.program Lexer.token linebuf in
    [%test_eq: Ast.program] ast (Declaration_list [])

let%test_unit "trivial main" =
    let source = "<?php // @pholyglot
    function main(): int {
        return 0;
    }
    " in
    let linebuf = Lexing.from_string source in
    let ast = Parser.program Lexer.token linebuf in
    [%test_eq: Ast.program] ast (Declaration_list [
        Function ("main", [], [
            Return (Num 0)
        ], Int)
    ])

(* Transpile from Pholly AST to polyglot AST *)
let%test_unit "trivial transpile" =
    let ast = Ast.Declaration_list [
        Function ("main", [], [
            Return (Num 0)
        ], Int)
    ] in
    let phast = Transpile.run ast in
    let pholyglot_code = Pholyglot_ast.string_of_program phast in
    [%test_eq: string] pholyglot_code {|//<?php echo "\x08\x08"; ob_start(); ?>
#define function 
//<?php
#__C__ int
function main()
{
    return 0;
}
// ?>
// <?php ob_end_clean(); main();|}

(* $a = 10; *)
(* $b = [1, 2, 3]; *)

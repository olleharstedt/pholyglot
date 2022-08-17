module Ast = Ast
module Lexer = Lexer
module Parser = Parser

module Transpile = struct
    let run (ast : Ast.program) : Pholyglot_ast.program =
        let open Pholyglot_ast in
        (
            Start_line,
            (* Include list *)
            [],
            (* Define list *)
            [
                Define ("function", None)
            ],
            (* Declarations *)
            [
            ],
            End_line
        )
end

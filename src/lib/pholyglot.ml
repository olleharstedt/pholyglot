module Test = struct
    type t = string

    let make () = "Hello"
end

module Ast = Ast
module Lexer = Lexer
module Parser = Parser

let a = 10

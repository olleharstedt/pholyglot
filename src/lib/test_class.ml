(**
 * Put all tests related to class and struct here.
 *)

open Base

let%test_unit "trivial class declare" =
    let source = {|<?php // @pholyglot
    class Point {
        public int $x;
        public float $y;
    }
    function main(): int {
        return 0;
    }
    |} in
    let linebuf = Lexing.from_string source in
    let ast = Parser.program Lexer.token linebuf in
    [%test_eq: Ast.program] ast (Declaration_list [
        Class {
            name= "Point";
            kind = Infer_kind;
            properties = [
                ("__object_property_x", Int);
                ("__object_property_y", Float);
            ];
            methods = [];
        };
        Function {
            name = "main";
            docblock = [];
            params = [];
            stmts = [
                Return (Num 0);
            ];
            function_type = Function_type {return_type = Int; arguments = []}
        }
    ])

let%test_unit "class new" =
    let source = {|<?php // @pholyglot
    class Point {
        public int $x;
        public int $y;
    }
    function main(): int {
        $p = new Point();
        return 0;
    }
    |} in
    let linebuf = Lexing.from_string source in
    let ast = Parser.program Lexer.token linebuf in
    [%test_eq: Ast.program] ast (Declaration_list [
        Class {
            name = "Point";
            kind = Infer_kind;
            properties = [
                ("__object_property_x", Int);
                ("__object_property_y", Int)
            ];
            methods = [];
        };
        Function {
            name = "main";
            docblock = [];
            params = [];
            stmts = [
                Assignment (Infer_me, Variable "p", (New (Class_type ("Point"), [])));
                Return (Num 0)
            ];
            function_type = Function_type {return_type = Int; arguments = []}
        }
    ])

let%test_unit "object lvalue assignment" =
    let source = {|<?php // @pholyglot
    class Point {
        public int $x;
        public int $y;
    }
    function main(): int {
        $p = new Point();
        $p->x = 1;
        return 0;
    }
    |} in
    let linebuf = Lexing.from_string source in
    let ast = Parser.program Lexer.token linebuf in
    [%test_eq: Ast.program] ast (Declaration_list [
        Class {
            name = "Point";
            kind = Infer_kind;
            properties = [
                ("__object_property_x", Int);
                ("__object_property_y", Int)
            ];
            methods = [];
        };
        Function {
            name = "main";
            docblock = [];
            params = [];
            stmts = [
                Assignment (Infer_me, Variable "p", (New (Class_type ("Point"), [])));
                Assignment (Infer_me, Object_access ("p", Property_access "__object_property_x"), (Num 1));
                Return (Num 0);
            ];
            function_type = Function_type {return_type = Int; arguments = []}
        }
    ])

let%test_unit "object object access in expression" =
    let source = {|<?php // @pholyglot
    class Point {
        public int $x;
        public int $y;
    }
    function main(): int {
        $p = new Point();
        $p->x = 1;
        printf("%d", $p->x);
        return 0;
    }
    |} in
    let linebuf = Lexing.from_string source in
    let ast = Parser.program Lexer.token linebuf in
    [%test_eq: Ast.program] ast (Declaration_list [
        Class {
            name = "Point";
            kind = Infer_kind;
            properties = [
                ("__object_property_x", Int);
                ("__object_property_y", Int)
            ];
            methods = [];
        };
        Function {
            name = "main";
            docblock = [];
            params = [];
            stmts = [
                Assignment (Infer_me, Variable "p", (New (Class_type ("Point"), [])));
                Assignment (Infer_me, Object_access ("p", Property_access "__object_property_x"), (Num 1));
                Function_call (Infer_me, "printf", [String "\"%d\""; Object_access (Variable "p", Property_access "__object_property_x")]);
                Return (Num 0);
            ];
            function_type = Function_type {return_type = Int; arguments = []}
        }
    ])

let%test_unit "infer object access" =
    let ns = Namespace.create () in
    let ast : Ast.program = Ast.Declaration_list [
        Class {
            name = "Point";
            kind = Infer_kind;
            properties = [
                ("x", Int);
                ("y", Int);
            ];
            methods = [];
        };
        Function {
            name = "main";
            docblock = [];
            params = [];
            stmts = [
                Assignment (Infer_me, Variable "p", (New (Class_type ("Point"), [])));
                Assignment (Infer_me, Object_access ("p", Property_access "x"), (Num 1));
                Function_call (Infer_me, "printf", [String "\"%d\""; Object_access (Variable "p", Property_access "x")]);
                Return (Num 0);
            ];
            function_type = Function_type {return_type = Int; arguments = []}
        }
    ] |> Infer.run ns in
    [%test_eq: Ast.program] ast (Declaration_list [
        Class {
            name = "Point";
            kind = Val;
            properties = [
                ("x", Int);
                ("y", Int);
            ];
            methods = [];
        };
        Function {
            name = "main";
            docblock = [];
            params = [];
            stmts = [
                Assignment (Class_type "Point", Variable "p", (New (Class_type ("Point"), [])));
                Assignment (Int, Object_access ("p", Property_access "x"), (Num 1));
                Function_call (
                    Function_type {return_type = Void; arguments = [String_literal; Int]},
                    "printf",
                    [Coerce (String_literal, String "\"%ld\""); Object_access (Variable "p", Property_access "x")]
                );
                Return (Num 0)
            ];
            function_type = Function_type {return_type = Int; arguments = []}
        }
    ])

let%test_unit "output object access" =
    let code = [
        Ast.Class {
            name = "Point";
            kind = Val;
            properties = [
                ("__object_property_x", Int);
                ("__object_property_y", Int)
            ];
            methods = [];
        };
        Ast.Function {
            name = "main";
            docblock = [];
            params = [];
            stmts = [
                Assignment (Class_type "Point", Variable "p", (New (Class_type "Point", [])));
                Assignment (Int, Object_access ("p", Property_access "__object_property_x"), (Num 1));
                Function_call (
                    Function_type {return_type = Void; arguments = [String_literal; Int]},
                    "printf",
                    [
                        Coerce (String_literal, String "\"%ld\"");
                        Object_access (Variable "p", Property_access "__object_property_x")
                    ]
                );
                Return (Num 0)
            ];
            function_type = Function_type {return_type = Int; arguments = []}
        }
    ]
         |> Transpile.declarations_to_pholyglot
         |> Pholyglot_ast.string_of_declares
    in
    [%test_eq: string] code {|
#__C__ typedef struct Point* Point;
class Point {
    #define public int
#define __object_property_x $__object_property_x
    public $__object_property_x;
#undef public
#define public int
#define __object_property_y $__object_property_y
    public $__object_property_y;
#undef public

    
// End of C struct def. Class methods are outside the struct.
#__C__ };

#if __PHP__
// End of PHP class def.
};
#endif
#if __PHP__
define("Point", "Point");
#endif
//?>
// Function pointer init
Point Point__constructor(Point $p)
{
    
    return $p;
}
//<?php
#define function int
function main()
#undef function
{
    #__C__ Point
    $p = new(Point);
    $p->__object_property_x = 1;
     printf("%ld", $p->__object_property_x);
    return 0;
}
|}

let%test_unit "string class property" =
    let source = {|<?php // @pholyglot
class Thing {
    public string $name;
}
function foo(Thing $t): void {
    printf("%s", $t->name);
}
|} in
    let linebuf = Lexing.from_string source in
    let ns = Namespace.create () in
    let ast = Parser.program Lexer.token linebuf |> Infer.run ns in
    [%test_eq: Ast.program] ast (Ast.Declaration_list [
        Class {
            name = "Thing";
            kind = Ref;
            properties = [
                ("__object_property_name", String);
            ];
            methods = [];
        };
        Function { 
            name = "foo";
            docblock = [];
            params = [Param ("t", Class_type "Thing")];
            stmts = [
                Function_call (
                    Function_type {return_type = Void; arguments = [String_literal; String_literal]},
                    "printf",
                    [Coerce (String_literal, String "\"%s\""); Coerce (String_literal, Object_access (Variable "t", Property_access "__object_property_name"))]
                );
            ];
            function_type = Function_type {return_type = Void; arguments = [Class_type "Thing"]}
        };
    ])

let%test "assign wrong object property" =
    let source = {|<?php // @pholyglot
class Point {
    public int $x;
}
function main(): int
{
    $p = new Point();
    $p->x = "moo";
    return 0;
}
|}
    in
    match
        Lexing.from_string source |>
        Parser.program Lexer.token |>
        Infer.run (Namespace.create ())
    with
    | exception Infer.Type_error _ -> true
    | _ -> false

let%test_unit "simple getter" =
    let source = "<?php // @pholyglot
class Point
{
    public int $x;
    public function getX(): int
    {
        return $this->x;
    }
}
    " in
    let ast =
        Lexing.from_string source |>
        Parser.program Lexer.token |>
        Infer.run (Namespace.create ())
    in
    [%test_eq: Ast.program] ast (Declaration_list [
        Class {
            name = "Point";
            kind = Val;
            properties = [("__object_property_x", Int)];
            methods = [
                {
                    name = "getX";
                    docblock = [];
                    params = [];
                    stmts = [
                        Return (Object_access (Variable "this", Property_access "__object_property_x"))
                    ];
                    function_type = Function_type {return_type = Int; arguments = []}
                }
            ]
        }
    ])

let%test_unit "simple getter pholyglot" =
    let code = Ast.Class {
            name = "Point";
            kind = Val;
            properties = [("__object_property_x", Int)];
            methods = [
                {
                    name = "getX";
                    docblock = [];
                    params = [];
                    stmts = [
                        Return (Object_access (Variable "this", Property_access "__object_property_x"))
                    ];
                    function_type = Function_type {return_type = Int; arguments = []}
                }
            ]
        }
         |> Transpile.declaration_to_pholyglot
         |> Pholyglot_ast.string_of_declare
    in

    let should_be = {|
#__C__ typedef struct Point* Point;
class Point {
    #define public int
#define __object_property_x $__object_property_x
    public $__object_property_x;
#undef public

    #__C__ int (*getX) (Point $__self); 
// End of C struct def. Class methods are outside the struct.
#__C__ };

#__C__ int Point__getX (Point $__self)
#if __PHP__
public function getX(Point $__self): int
#endif
{
    return $__self->__object_property_x;

}

#if __PHP__
// End of PHP class def.
};
#endif
#if __PHP__
define("Point", "Point");
#endif
//?>
// Function pointer init
Point Point__constructor(Point $p)
{
    $p->getX = &Point__getX;

    return $p;
}
//<?php
|}
    in

    [%test_eq: string] code should_be

let%test_unit "offsetMomentum method" =
    let source = "<?php // @pholyglot
class Body
{
    public float $vx;
    public function offsetMomentum(float $px): void
    {
        $solarmass = 10.;
        $this->vx = $px / $solarmass;
    }
}
    " in
    let ast =
        Lexing.from_string source |>
        Parser.program Lexer.token |>
        Infer.run (Namespace.create ())
    in
    [%test_eq: Ast.program] ast (Declaration_list [
        Class {
            name = "Body";
            kind = Val;
            properties = [("__object_property_vx", Float)];
            methods = [
                {
                    name = "offsetMomentum";
                    docblock = [];
                    params = [Param ("px", Float)];
                    stmts = [
                        Assignment (Float, Variable "solarmass", Num_float 10.);
                        Assignment (
                            Float,
                            Object_access ("this", (Property_access "__object_property_vx")),
                            Div ((Variable "px"), (Variable "solarmass")))
                    ];
                    function_type = Function_type {return_type = Void; arguments = [Float]}
                }
            ]
        }
    ])

let%test_unit "offsetMomentum method pholyglot" =
    let ast : Ast.declaration =
        Class {
            name = "Body";
            kind = Val;
            properties = [("__object_property_vx", Float)];
            methods = [
                {
                    name = "offsetMomentum";
                    docblock = [];
                    params = [Param ("px", Float)];
                    stmts = [
                        Assignment (Float, Variable "solarmass", Num_float 10.);
                        Assignment (
                            Float,
                            Object_access ("this", (Property_access "__object_property_vx")),
                            Div ((Variable "px"), (Variable "solarmass")))
                    ];
                    function_type = Function_type {return_type = Void; arguments = [Float]}
                }
            ]
        }
    in
    let phast = Transpile.declaration_to_pholyglot ast in
    let code = Pholyglot_ast.string_of_declare phast in
    let should_be = {|
#__C__ typedef struct Body* Body;
class Body {
    #define public float
#define __object_property_vx $__object_property_vx
    public $__object_property_vx;
#undef public

    #__C__ void (*offsetMomentum) (Body $__self, float $px); 
// End of C struct def. Class methods are outside the struct.
#__C__ };

#__C__ void Body__offsetMomentum (Body $__self, float $px)
#if __PHP__
public function offsetMomentum(Body $__self, float $px): void
#endif
{
    #__C__ float
    $solarmass = 10.;
    $__self->__object_property_vx = $px / $solarmass;
    
}

#if __PHP__
// End of PHP class def.
};
#endif
#if __PHP__
define("Body", "Body");
#endif
//?>
// Function pointer init
Body Body__constructor(Body $p)
{
    $p->offsetMomentum = &Body__offsetMomentum;

    return $p;
}
//<?php
|}
    in

    (*
    for i = 0 to String.length code do
        print_char code.[i];
        if code.[i] != should_be.[i] then begin
            print_endline "\nGot:";
            print_int (Caml.Char.code code.[i]);
            print_endline "\nExpected:";
            print_int (Caml.Char.code should_be.[i]);
            exit 1;
        end;
    done;
    *)

    (*
    open Patdiff_kernel
    module Patdiff_core = Patdiff_core.Without_unix
    let keep_ws = false in
    let hunks = Patdiff_core.diff
        ~context:Configuration.default_context
        ~line_big_enough:Configuration.default_line_big_enough
        ~keep_ws
        ~prev:[| "hello"; "world" |]
        ~next:[| "good bye"; "world" |]
    in
    *)

    [%test_eq: string] code should_be

let%test "return ref type is invalid" =
    let source = {|<?php // @pholyglot
class User {
    public string $name;
}
function foo(): User
{
    $p = new User();
    return $p;
}
|} in
    let ns = Namespace.create () in
    let ast = try 
        ignore(Lexing.from_string source |>
        Parser.program Lexer.token |>
        Infer.run ns);
        false
    with
         | Infer.Type_error _ -> true
         | _ -> false
    in ast

let%test "return val type is valid" =
    let source = {|<?php // @pholyglot
class Point {
    public int $x;
}
function foo(): Point
{
    $p = new Point();
    return $p;
}
|} in
    let ns = Namespace.create () in
    (*let res = try *)
    let ast = try 
        ignore(Lexing.from_string source |>
        Parser.program Lexer.token |>
        Infer.run ns);
        true
    with
         | Infer.Type_error _ -> false
         | _ -> true
    in ast

let%test_unit "infer method" =
    let source = {|<?php // @pholyglot
class Point
{
    public int $x;
    public function getX(): int
    {
        printf("Hello");
        return $this->x;
    }
}
    |} in
    let ast =
        Lexing.from_string source |>
        Parser.program Lexer.token |>
        Infer.run (Namespace.create ())
    in
    [%test_eq: Ast.program] ast (Declaration_list [
        Class {
            name = "Point";
            kind = Val;
            properties = [("__object_property_x", Int)];
            methods = [
                {
                    name = "getX";
                    docblock = [];
                    params = [];
                    stmts = [
                        Function_call (
                            Function_type {return_type = Void; arguments = [String_literal]},
                            "printf",
                            [Coerce (String_literal, String "\"Hello\"")]
                        );
                        Return (Object_access (Variable "this", Property_access "__object_property_x"))
                    ];
                    function_type = Function_type {return_type = Int; arguments = []}
                }
            ]
        }
    ])

let%test_unit "infer method call" =
    let ns = Namespace.create () in
    let c : Ast.declaration = Class {
        name = "Point";
        kind = Val;
        properties = [];
        methods = [
            {
                name          = "getX";
                docblock      = [];
                params        = [];
                stmts         = [];
                function_type = Function_type {return_type = Int; arguments = []}
            }
        ];
    } in
    Namespace.add_class_type ns c;
    Namespace.add_identifier ns "p" (Class_type "Point");
    let expr : Ast.expression = Object_access (Variable "p", Method_call {return_type = Infer_me; method_name = "getX"; args = []; left_hand = Variable "p"}) in
    let ast = Infer.infer_expression ns expr in
    [%test_eq: Ast.expression] ast (Object_access (Variable "p", Method_call {return_type = Int; method_name = "getX"; args = []; left_hand = Variable "p"}))

let%test_unit "infer method" =
    let source = {|<?php // @pholyglot
class Point
{
    public int $x;
    public function getX(): int
    {
        printf("Hello");
        return $this->x;
    }
}
function main(): int
{
    $p = new Point();
    printf("%d", $p->getX());
    return 0;
}
    |} in
    let ast =
        Lexing.from_string source |>
        Parser.program Lexer.token |>
        Infer.run (Namespace.create ())
    in
    [%test_eq: Ast.program] ast (Declaration_list [
        Class {
            name = "Point";
            kind = Val;
            properties = [("__object_property_x", Int)];
            methods = [
                {
                    name = "getX";
                    docblock = [];
                    params = [];
                    stmts = [
                        Function_call (
                            Function_type {return_type = Void; arguments = [String_literal]},
                            "printf",
                            [Coerce (String_literal, String "\"Hello\"")]
                        );
                        Return (Object_access (Variable "this", Property_access "__object_property_x"))
                    ];
                    function_type = Function_type {return_type = Int; arguments = []}
                }
            ]
        };
        Function {
            name = "main";
            docblock = [];
            params = [];
            stmts = [
                Assignment (Class_type "Point", Variable "p", (New (Class_type ("Point"), [])));
                Function_call (
                    Function_type {return_type = Void; arguments = [String_literal; Int]},
                    "printf",
                    [
                        Coerce (String_literal, String "\"%ld\"");
                        Method_call {return_type = Int; method_name = "getX"; args = []; left_hand = Variable "p"};
                    ]
                );
                Return (Num 0)
            ];
            function_type = Function_type {return_type = Int; arguments = []}
        }
    ])

let%test_unit "transpile method" =
    let ast : Ast.expression =
        Function_call (
            Function_type {return_type = Void; arguments = [String_literal; Int]},
            "printf",
            [
                Coerce (String_literal, String "\"%ld\"");
                Method_call {
                    return_type = Int;
                    method_name = "getX";
                    args = [
                        Variable "var1";
                        Function_call (
                            Function_type {return_type = Void; arguments = []},
                            "moo",
                            []
                        );
                        Function_call (
                            Function_type {return_type = Int; arguments = [Constant; Dynamic_array Int; Int]},
                            "array_get",
                            [Constant "int"; Variable "arr"; Num 0]
                        );
                    ];
                    left_hand = Variable "p";
                }
            ]
        )
    in
    let phast = Transpile.expression_to_pholyglot ast in
    let code = Pholyglot_ast.string_of_expression phast in
    [%test_eq: string] code {|pprintf("%ld", $p->getX($p, $var1, moo(), array_get(int, $arr, 0)))|}

let%test_unit "object access inside array access" =
    let source = {|<?php // @pholyglot
    class Body {public int $x;}
    function foo(): void {
        $b = new Body();
        $b->x = 10;
        $arr = [$b];
        $x = $arr[0]->x;
        printf("%d", $arr[0]->x);
    }
    |} in
    let ast =
        Lexing.from_string source |>
        Parser.program Lexer.token |>
        Infer.run (Namespace.create ())
    in
    [%test_eq: Ast.program] ast (Declaration_list [
        Class {
            name = "Body";
            kind = Val;
            properties = [("__object_property_x", Int)];
            methods = []
        };
        Function {
            name = "foo";
            docblock = [];
            params = [];
            stmts = [
                Assignment (Class_type "Body", Variable "b", New (Class_type "Body", []));
                Assignment (Int, Object_access ("b", (Property_access "__object_property_x")), (Num 10));
                Assignment (Fixed_array (Class_type "Body", Some 1), (Variable "arr"),
                    Function_call (
                        Function_type {return_type = Fixed_array (Class_type "Body", Some 1); arguments = [Constant; Int; Class_type "Body"]},
                        "array_make",
                        [Constant "Body"; Num 1; Variable "b"];
                    )
                );
                Assignment (Int, Variable "x", Object_access (
                    Function_call (
                        Function_type {return_type = Class_type "Body"; arguments = [Constant; Dynamic_array (Class_type "Body"); Int]},
                        "array_get",
                        [Constant "Body"; Variable "arr"; Num 0]
                    ),
                    Property_access "__object_property_x")
                );
                Function_call (
                    Function_type {return_type = Void; arguments = [String_literal; Int]},
                    "printf",
                    [
                        Coerce (String_literal, String "\"%ld\"");
                        Object_access(
                            Function_call (
                                Function_type {return_type = Class_type "Body"; arguments = [Constant; Dynamic_array (Class_type "Body"); Int]},
                                "array_get",
                                [Constant "Body"; Variable "arr"; Num 0]
                            ),
                            Property_access "__object_property_x"
                        )
                    ]
                );
            ];
            function_type = Function_type {return_type = Void; arguments = []}
        }
    ])

let%test_unit "object access inside array access transpile" =
    let fn : Ast.declaration = Function {
            name = "foo";
            docblock = [];
            params = [];
            stmts = [
                Assignment (Class_type "Body", Variable "b", New (Class_type "Body", []));
                Assignment (Int, Object_access ("b", (Property_access "__object_property_x")), (Num 10));
                Assignment (Fixed_array (Class_type "Body", Some 1), (Variable "arr"),
                    Function_call (
                        Function_type {return_type = Fixed_array (Class_type "Body", Some 1); arguments = [Constant; Int; Class_type "Body"]},
                        "array_make",
                        [Constant "Body"; Num 1; Variable "b"];
                    )
                );
                Assignment (Int, Variable "x", Object_access (
                    Function_call (
                        Function_type {return_type = Int; arguments = [Constant; Dynamic_array (Class_type "Body"); Int]},
                        "array_get",
                        [Constant "Body"; Variable "arr"; Num 0]
                    ),
                    Property_access "__object_property_x"));
            ];
            function_type = Function_type {return_type = Void; arguments = []}
        }
    in
    let phast = Transpile.declaration_to_pholyglot fn in
    let code = Pholyglot_ast.string_of_declare phast in
	(* TODO: Check this code *)
    [%test_eq: string] code {|#define function void
function foo()
#undef function
{
    #__C__ Body
    $b = new(Body);
    $b->__object_property_x = 10;
    #__C__ array
    $arr = array_make(Body, 1, $b);
    #__C__ int
    $x = array_get(Body, $arr, 0)->__object_property_x;
    }
|}

let%test_unit "method call as statement" =
    let source = {|<?php // @pholyglot
class Body {
    public function doSomething(int $x): void { }
}
function foo(): void {
    $x = 10;
    $b = new Body();
    $b->doSomething($x);
}
    |} in
    let ast =
        Lexing.from_string source |>
        Parser.program Lexer.token |>
        Infer.run (Namespace.create ())
    in
    [%test_eq: Ast.program] ast (Declaration_list [
        Class {
            name = "Body";
            kind = Val;
            properties = [];
            methods = [
                {
                    name = "doSomething";
                    docblock = [];
                    params = [Param ("x", Int)];
                    stmts = [];
                    function_type = Function_type {return_type = Void; arguments = [Int]}
                }
            ]
        };
        Function { 
            name = "foo";
            docblock = [];
            params = [];
            stmts = [
                Assignment (Int, Variable "x", Num 10);
                Assignment (Class_type "Body", Variable "b", New (Class_type "Body", []));
                Method_call {
                    lvalue = Object_access ("b", Property_access "__object_property_doSomething");
                    args   = [Variable "x"];
                }
            ];
            function_type = Function_type {return_type = Void; arguments = []};
        };
    ])
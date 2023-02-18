open OUnit2

let tests = "assignment new" >::: [
    "mo" >:: (fun _ -> assert_equal 1 1)
]

let _ = run_test_tt_main tests

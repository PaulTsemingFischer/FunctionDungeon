open OUnit2
open Engine
open Game

let tests = "test suite" >::: []
let _ = run_test_tt_main tests

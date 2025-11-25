(** Copyright 2025-2025, Lab2 Ryazanov Demid 367522 *)

(** SPDX-License-Identifier: NONE *)

open OUnit2

let () =
  run_test_tt_main
    ("All Tests"
    >::: [ Btdict_test.suite; "Property Tests" >::: Btdict_props.suite_props ])

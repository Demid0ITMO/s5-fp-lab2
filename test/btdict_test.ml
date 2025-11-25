(** Copyright 2025-2025, Lab2 Ryazanov Demid 367522 *)

(** SPDX-License-Identifier: NONE *)

open OUnit2
open Btdict

let test_insert_lookup _ =
  let d = from_list [ (3, "c"); (1, "a"); (2, "b") ] in
  assert_equal (Some "a") (lookup 1 d);
  assert_equal (Some "b") (lookup 2 d);
  assert_equal (Some "c") (lookup 3 d);
  assert_equal None (lookup 4 d)

let test_insert_update _ =
  let d1 = from_list [ (1, "a"); (2, "b") ] in
  let d2 = insert 2 "B" d1 in
  assert_equal (Some "B") (lookup 2 d2)

let test_delete _ =
  let d = from_list [ (1, "a"); (2, "b"); (3, "c") ] in
  let d' = delete 3 d in
  assert_equal None (lookup 3 d');
  assert_equal [ 1; 2 ] (keys d')

let test_delete_one_child _ =
  let d = from_list [ (2, "b"); (1, "a") ] in
  let d' = delete 2 d in
  assert_equal None (lookup 2 d');
  assert_equal [ 1 ] (keys d')

let test_delete_two_children _ =
  let d =
    from_list [ (5, "e"); (3, "c"); (7, "g"); (6, "f"); (8, "h"); (4, "d") ]
  in
  let d' = delete 5 d in
  assert_equal None (lookup 5 d');
  assert_equal [ 3; 4; 6; 7; 8 ] (keys d')

let test_filter _ =
  let d = from_list [ (1, "a"); (2, "bb"); (3, "ccc"); (4, "dd") ] in
  let p (k, _) = k mod 2 = 0 in
  let d' = filter p d in
  assert_equal [ 2; 4 ] (keys d');
  assert_equal [ "bb"; "dd" ] (values d')

let test_map_values _ =
  let d = from_list [ (1, 10); (2, 20); (3, 30) ] in
  let d' = map_values (( + ) 1) d in
  assert_equal [ 1; 2; 3 ] (keys d');
  assert_equal [ 11; 21; 31 ] (values d')

let test_folds _ =
  let d = from_list [ (1, 1); (2, 2); (3, 3) ] in
  let sum = fold_left (fun acc (_, v) -> acc + v) 0 d in
  assert_equal 6 sum;

  let d_char = from_list [ (1, 'a'); (2, 'b'); (3, 'c') ] in
  let asc = fold_right (fun x acc -> x :: acc) d_char [] in
  assert_equal [ (1, 'a'); (2, 'b'); (3, 'c') ] asc

let test_monoid _ =
  let a = from_list [ (1, "a"); (2, "x") ] in
  let b = from_list [ (2, "b"); (3, "c") ] in
  let c = union a b in
  assert_equal [ (1, "a"); (2, "b"); (3, "c") ] (to_asc_list c);

  let d = from_list [ (1, 10); (2, 20) ] in
  assert_equal (to_asc_list d) (to_asc_list (union d neutral));
  assert_equal (to_asc_list d) (to_asc_list (union neutral d))

let test_equals _ =
  let a = from_list [ (2, 'b'); (1, 'a'); (3, 'c') ] in
  let b = from_list [ (1, 'a'); (3, 'c'); (2, 'b') ] in
  assert_equal true (equals a b)

let suite =
  "BTDict Suite"
  >::: [
         "insert/lookup" >:: test_insert_lookup;
         "insert update" >:: test_insert_update;
         "delete leaf" >:: test_delete;
         "delete one child" >:: test_delete_one_child;
         "delete two children" >:: test_delete_two_children;
         "filter" >:: test_filter;
         "map_values" >:: test_map_values;
         "folds" >:: test_folds;
         "monoid" >:: test_monoid;
         "equals" >:: test_equals;
       ]

let () = run_test_tt_main suite

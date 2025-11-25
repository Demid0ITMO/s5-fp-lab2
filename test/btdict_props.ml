(** Copyright 2025-2025, Lab2 Ryazanov Demid 367522 *)

(** SPDX-License-Identifier: NONE *)

open QCheck
open Btdict

let arb_pairs = Gen.(list (pair int int)) |> make

let arb_dict =
  let gen_dict = Gen.(map from_list (list (pair int int))) in
  make gen_dict

let to_asc_list = to_asc_list
let eq_by_list a b = to_asc_list a = to_asc_list b
let member k d = match lookup k d with Some _ -> true | None -> false

let monoid_assoc =
  Test.make ~name:"monoid_assoc" (triple arb_dict arb_dict arb_dict)
    (fun (a, b, c) -> eq_by_list (union (union a b) c) (union a (union b c)))

let monoid_left_id =
  Test.make ~name:"monoid_left_id" arb_dict (fun a ->
      eq_by_list (union neutral a) a)

let monoid_right_id =
  Test.make ~name:"monoid_right_id" arb_dict (fun a ->
      eq_by_list (union a neutral) a)

let insert_lookup_prop =
  Test.make ~name:"insert_lookup"
    (pair arb_dict (pair int int))
    (fun (d, (k, v)) -> lookup k (insert k v d) = Some v)

let delete_prop =
  Test.make ~name:"delete" (pair arb_dict int) (fun (d, k) ->
      not (member k (delete k d)))

let sort_prop =
  Test.make ~name:"sort" arb_dict (fun d ->
      let lst = to_asc_list d in
      let sorted = List.sort (fun (k1, _) (k2, _) -> compare k1 k2) lst in
      lst = sorted)

let roundtrip_prop =
  Test.make ~name:"roundtrip" arb_dict (fun d ->
      eq_by_list (from_list (to_asc_list d)) d)

let map_values_prop =
  Test.make ~name:"map_values" arb_dict (fun d ->
      let d' = map_values (( + ) 1) d in
      keys d' = keys d && values d' = List.map (( + ) 1) (values d))

let filter_prop =
  Test.make ~name:"filter" arb_dict (fun d ->
      let p (k, _) = k mod 2 = 0 in
      let d' = filter p d in
      List.for_all p (to_asc_list d'))

let eq_prop =
  Test.make ~name:"eq" (pair arb_dict arb_dict) (fun (a, b) ->
      equals a b = eq_by_list a b)

let suite_props =
  List.map QCheck_ounit.to_ounit2_test
    [
      monoid_assoc;
      monoid_left_id;
      monoid_right_id;
      insert_lookup_prop;
      delete_prop;
      sort_prop;
      roundtrip_prop;
      map_values_prop;
      filter_prop;
      eq_prop;
    ]

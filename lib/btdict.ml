(** Copyright 2025-2025, Lab2 Ryazanov Demid 367522 *)

(** SPDX-License-Identifier: NONE *)

type ('k, 'v) btdict =
  | Empty
  | Node of ('k, 'v) btdict * ('k * 'v) * ('k, 'v) btdict

let empty = Empty

let rec find_min = function
  | Node (Empty, kv, _) -> Some kv
  | Node (left, _, _) -> find_min left
  | Empty -> None

let rec merge l r =
  match (l, r) with
  | Empty, _ -> r
  | _, Empty -> l
  | _ -> (
      match find_min r with
      | Some (k_min, v_min) -> Node (l, (k_min, v_min), delete k_min r)
      | None -> l)

and insert k v = function
  | Empty -> Node (Empty, (k, v), Empty)
  | Node (left, (k', v'), right) -> (
      match k with
      | k when k < k' -> Node (insert k v left, (k', v'), right)
      | k when k > k' -> Node (left, (k', v'), insert k v right)
      | _ -> Node (left, (k, v), right))

and delete k = function
  | Empty -> Empty
  | Node (left, (k', v'), right) -> (
      match k with
      | k when k < k' -> Node (delete k left, (k', v'), right)
      | k when k > k' -> Node (left, (k', v'), delete k right)
      | _ -> (
          match (left, right) with
          | Empty, Empty -> Empty
          | Empty, _ -> right
          | _, Empty -> left
          | _ -> (
              match find_min right with
              | Some (k_min, v_min) ->
                  Node (left, (k_min, v_min), delete k_min right)
              | None -> left)))

let rec lookup k = function
  | Empty -> None
  | Node (left, (k', v'), right) -> (
      match k with
      | k when k < k' -> lookup k left
      | k when k > k' -> lookup k right
      | _ -> Some v')

let rec map_values f = function
  | Empty -> Empty
  | Node (left, (k', v'), right) ->
      Node (map_values f left, (k', f v'), map_values f right)

let rec filter f = function
  | Empty -> Empty
  | Node (left, (k', v'), right) when f (k', v') ->
      Node (filter f left, (k', v'), filter f right)
  | Node (left, (_, _), right) -> merge (filter f left) (filter f right)

let rec fold_left f acc = function
  | Empty -> acc
  | Node (left, kv, right) ->
      let acc_left = fold_left f acc left in
      let acc_node = f acc_left kv in
      fold_left f acc_node right

let rec fold_right f dict acc =
  match dict with
  | Empty -> acc
  | Node (left, kv, right) ->
      let acc_right = fold_right f right acc in
      let acc_node = f kv acc_right in
      fold_right f left acc_node

let from_list lst = List.fold_left (fun acc (k, v) -> insert k v acc) empty lst
let to_asc_list dict = fold_right (fun x acc -> x :: acc) dict []
let keys dict = List.map fst (to_asc_list dict)
let values dict = List.map snd (to_asc_list dict)

let equals dict1 dict2 =
  let rec push_left stack = function
    | Empty -> stack
    | Node (l, (k, v), r) -> push_left ((k, v, r) :: stack) l
  in
  let next = function
    | [] -> None
    | (k, v, r) :: rest -> Some ((k, v), push_left rest r)
  in
  let rec go s1 s2 =
    match (next s1, next s2) with
    | None, None -> true
    | Some (kv1, s1'), Some (kv2, s2') -> kv1 = kv2 && go s1' s2'
    | _ -> false
  in
  go (push_left [] dict1) (push_left [] dict2)

let union dict1 dict2 = fold_left (fun acc (k, v) -> insert k v acc) dict1 dict2
let neutral = empty

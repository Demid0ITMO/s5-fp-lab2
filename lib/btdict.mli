(** Copyright 2025-2025, Lab2 Ryazanov Demid 367522 *)

(** SPDX-License-Identifier: NONE *)

type ('k, 'v) btdict

val empty : ('k, 'v) btdict
val insert : 'k -> 'v -> ('k, 'v) btdict -> ('k, 'v) btdict
val delete : 'k -> ('k, 'v) btdict -> ('k, 'v) btdict
val lookup : 'k -> ('k, 'v) btdict -> 'v option
val map_values : ('v -> 'v) -> ('k, 'v) btdict -> ('k, 'v) btdict
val filter : ('k * 'v -> bool) -> ('k, 'v) btdict -> ('k, 'v) btdict
val fold_left : ('a -> 'k * 'v -> 'a) -> 'a -> ('k, 'v) btdict -> 'a
val fold_right : ('k * 'v -> 'a -> 'a) -> ('k, 'v) btdict -> 'a -> 'a
val from_list : ('k * 'v) list -> ('k, 'v) btdict
val to_asc_list : ('k, 'v) btdict -> ('k * 'v) list
val keys : ('k, 'v) btdict -> 'k list
val values : ('k, 'v) btdict -> 'v list
val equals : ('k, 'v) btdict -> ('k, 'v) btdict -> bool
val union : ('k, 'v) btdict -> ('k, 'v) btdict -> ('k, 'v) btdict
val neutral : ('k, 'v) btdict

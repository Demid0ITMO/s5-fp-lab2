# Лабораторная работа №2

#### Студент `Рязанов Демид Витальевич`
#### ИСУ `367522`
#### Вариант `bt-dict`
#### Язык `Ocaml`

---

## Требования

1. Функции:
  - добавление и удаление элементов;
  - фильтрация;
  - отображение (map);
  - свертки (левая и правая);
  - структура должна быть моноидом.
2. Структуры данных должны быть неизменяемыми.
3. Библиотека должна быть протестирована в рамках unit testing.
4. Библиотека должна быть протестирована в рамках property-based тестирования
(как минимум 3 свойства, включая свойства моноида).
5. Структура должна быть полиморфной.
6. Требуется использовать идиоматичный для технологии стиль программирования.

## Реализация
```ocaml
type ('k, 'v) btdict =
  | Empty
  | Node of ('k, 'v) btdict * ('k * 'v) * ('k, 'v) btdict
```

### Функции
```ocaml
(* ДОБАВЛЕНИЕ *)
let rec insert k v = function
  | Empty -> Node (Empty, (k, v), Empty)
  | Node (left, (k', v'), right) ->
      match k with
      | k when k < k' -> Node (insert k v left, (k', v'), right)
      | k when k > k' -> Node (left, (k', v'), insert k v right)
      | _ -> Node (left, (k, v), right)

(* УДАЛЕНИЕ *)
let rec delete k = function
  | Empty -> Empty
  | Node (left, (k', v'), right) ->
      match k with
      | k when k < k' -> Node (delete k left, (k', v'), right)
      | k when k > k' -> Node (left, (k', v'), delete k right)
      | _ -> 
          match (left, right) with
          | Empty, Empty -> Empty
          | Empty, _ -> right
          | _, Empty -> left
          | _ -> 
              match find_min right with
              | Some (k_min, v_min) ->
                  Node (left, (k_min, v_min), delete k_min right)
              | None -> left

(* ПОИСК *)
let rec lookup k = function
  | Empty -> None
  | Node (left, (k', v'), right) ->
      match k with
      | k when k < k' -> lookup k left
      | k when k > k' -> lookup k right
      | _ -> Some v'

(* ОТОБРАЖЕНИЕ *)
let rec map_values f = function
  | Empty -> Empty
  | Node (left, (k', v'), right) ->
      Node (map_values f left, (k', f v'), map_values f right)

(* ФИЛЬТРАЦИЯ *)
let rec filter f = function
  | Empty -> Empty
  | Node (left, (k', v'), right) when f (k', v') ->
      Node (filter f left, (k', v'), filter f right)
  | Node (left, (_, _), right) -> merge (filter f left) (filter f right)

(* ЛЕВАЯ СВЕРТКА *)
let rec fold_left f acc = function
  | Empty -> acc
  | Node (left, kv, right) ->
      let acc_left = fold_left f acc left in
      let acc_node = f acc_left kv in
      fold_left f acc_node right

(* ПРАВАЯ СВЕРТКА *)
let rec fold_right f dict acc =
  match dict with
  | Empty -> acc
  | Node (left, kv, right) ->
      let acc_right = fold_right f right acc in
      let acc_node = f kv acc_right in
      fold_right f left acc_node
```

### Функции моноида 
```ocaml
(* ОПЕРАЦИЯ АССОЦИАТИВНОСТИ *)
let union dict1 dict2 = 
  fold_left (fun acc (k, v) -> insert k v acc) dict1 dict2

(* НЕЙТРАЛЬНЫЙ ЭЛЕМЕНТ *)
let neutral = empty
```

## Тесты

[Unit](./test/btdict_test.ml)

[Property-based](./test/btdict_props.ml)

## Вывод
В ходе выполнения задач была реализована необходимая структура.
Для структуры реализованы все необходимые функции, структура является
полиморфной, неизменяемой, моноидом. 
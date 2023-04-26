(** Code for [empty], [is_empty], [size], [insert], [find], [fold], and
    [to_list] largely copied from Joel Valerio (jev66)'s Assignment 3. Balance
    is from textbook. *)

open Consts

exception InvalidPosition of string

(** The type representing the color of a node in a rbtree. *)
type color =
  | Red
  | Black

(** The type representing a red-black tree. It satisfies all Red-Black tree
    invariants. *)
type ('k, 'v) rbtree =
  | Node of color * ('k * 'v) * ('k, 'v) rbtree * ('k, 'v) rbtree
  | Leaf

type ship = {
  length : int;
  adjacents : (int * int) list;
}

type cell =
  | Empty
  | Ship of { ship : ship ref }
  | Hit
  | Miss

type board = (int * int, cell) rbtree

let ( @<< ) (x, y) (a, b) =
  if y < b then true
  else if y > b then false
  else if x < a then true
  else if x > a then false
  else false

let ( >>@ ) (x, y) (a, b) =
  if y < b then false
  else if y > b then true
  else if x < a then false
  else if x > a then true
  else false

(** [empty] is the board with no cells. *)
let empty = Leaf

(** [is_empty d] is whether [d] is the board with no cells. *)
let is_empty d = d = Leaf

(** [size d] is the number of bindings in [d]. *)
let rec size d =
  match d with
  | Leaf -> 0
  | Node (_, _, l, r) -> 1 + size l + size r

(** [balance t] balances the tree with root Node [t]. *)
let balance = function
  | Black, z, Node (Red, y, Node (Red, x, a, b), c), d
  | Black, z, Node (Red, x, a, Node (Red, y, b, c)), d
  | Black, x, a, Node (Red, z, Node (Red, y, b, c), d)
  | Black, x, a, Node (Red, y, b, Node (Red, z, c, d)) ->
      Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
  | a, b, c, d -> Node (a, b, c, d)

let insert k v d =
  let rec ins = function
    | Leaf -> Node (Red, (k, v), Leaf, Leaf)
    | Node (color, (k', v'), a, b) ->
        if k @<< k' then balance (color, (k', v'), ins a, b)
        else if k >>@ k' then balance (color, (k', v'), a, ins b)
        else Node (color, (k, v), a, b)
  in
  let output =
    match ins d with
    | Node (_, y, a, b) -> Node (Black, y, a, b)
    | Leaf -> failwith "RBT insert failed"
  in
  output

let rec find k d =
  match d with
  | Leaf -> None
  | Node (_, (k', v'), l, r) ->
      if k @<< k' then find k l else if k >>@ k' then find k r else Some v'

let rec fold f acc d =
  match d with
  | Leaf -> acc
  | Node (_, (k, v), l, r) -> fold f (f k v (fold f acc l)) r

let to_list d = fold (fun k v acc -> (k, v) :: acc) [] d |> List.rev

let init_board () =
  let rec helper_y y acc =
    let rec helper_x x acc =
      match x with
      | -1 -> acc
      | _ -> helper_x (x - 1) (insert (x, y) Empty acc)
    in
    match y with
    | -1 -> acc
    | _ -> helper_y (y - 1) (fold insert acc (helper_x (board_size - 1) empty))
  in
  helper_y (board_size - 1) empty

let init_ship length = { length; adjacents = [] }
let string_of_coord (x, y) = "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ")"

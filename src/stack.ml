type 'a t = 'a list

exception EMPTY

let empty = []

let is_empty = function
  | [] -> true
  | _ -> false

let push = List.cons

let peek = function
  | [] -> raise EMPTY
  | x :: _ -> x

let pop = function
  | [] -> raise EMPTY
  | _ :: s -> s

let size = List.length
let to_list = Fun.id
let of_list = Fun.id
let append = List.append
let rec rem_elements elems t = List.filter (fun x -> not (List.mem x elems)) t

let of_array arr =
  let rec of_array_helper acc = function
    | -1 -> acc
    | i -> of_array_helper (arr.(i) :: acc) (i - 1)
  in
  of_array_helper empty (Array.length arr - 1)

let string_of_stack f s =
  let s = List.map f s in
  let ( +^+ ) a b = Buffer.add_string a b in
  let buff = Buffer.create 100 in
  Buffer.add_char buff '[';
  List.iter
    (fun e ->
      buff +^+ e;
      Buffer.add_char buff ';')
    s;
  Buffer.add_char buff ']';
  Buffer.contents buff

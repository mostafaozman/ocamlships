module type Stack = sig
  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val push : 'a -> 'a t -> 'a t
  val peek : 'a t -> 'a option
  val pop : 'a t -> 'a t option
  val size : 'a t -> int
  val to_list : 'a t -> 'a list
  val of_array : 'a array -> 'a t
end

module Stack : Stack = struct
  type 'a t = 'a list

  exception Empty

  let empty = []

  let is_empty = function
    | [] -> true
    | _ -> false

  let push = List.cons

  let peek = function
    | [] -> None
    | x :: _ -> Some x

  let pop = function
    | [] -> None
    | _ :: s -> Some s

  let size = List.length
  let to_list = Fun.id

  let of_array arr =
    let rec of_array_helper acc = function
      | -1 -> acc
      | i -> of_array_helper (arr.(i) :: acc) (i - 1)
    in
    of_array_helper empty (Array.length arr)
end

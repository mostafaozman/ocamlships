type 'a t
(** the type representing a stack. *)

exception EMPTY

val empty : 'a t
(** The empty stack *)

val is_empty : 'a t -> bool
(** [is_empty t] is whether [t] is empty. *)

val push : 'a -> 'a t -> 'a t
(** [push x t] adds [x] to the stack [t]. *)

val peek : 'a t -> 'a
(** [peek t] is the element on top of the stack. Raises: Empty if the stack is
    empty. *)

val pop : 'a t -> 'a t
(** [pop t] is [t] with its topmost element removed. Raises: Empty if the stack
    is empty. *)

val size : 'a t -> int
(** [size t] is the number of elements in [t]. *)

val rem_elements : 'a list -> 'a t -> 'a t
(** [rem_elements e t] is [t] with all elements also in [e] removed. *)

val to_list : 'a t -> 'a list
(** [to_list t] is the list representation of stack [t]. The topmost element of
    [t] is the first element of the list. *)

val of_list : 'a list -> 'a t

val of_array : 'a array -> 'a t
(** [of_array arr] is the stack consisting of [arr]'s elements. The first
    element of [arr] is the topmost element of the stack. *)

(** Stack is the module that provides a functional stack data structure. *)

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

val append : 'a t -> 'a t -> 'a t
(** [append t1 t2] is [t2] appended to [t1]. First element of [t1] is at the top
    of the stack. *)

val rem_elements : 'a list -> 'a t -> 'a t
(** [rem_elements e t] is [t] with all elements also in [e] removed. *)

val to_list : 'a t -> 'a list
(** [to_list t] is the list representation of stack [t]. The topmost element of
    [t] is the first element of the list. *)

val of_list : 'a list -> 'a t
(** [of_list l] is the stack consiting of [l]'s elements. The first element of
    [l] is the topmost element of the stack. *)

val of_array : 'a array -> 'a t
(** [of_array arr] is the stack consisting of [arr]'s elements. The first
    element of [arr] is the topmost element of the stack. *)

val string_of_stack : ('a -> string) -> 'a t -> string
(** [string_of_stack f t] is a string representation of t. Requires f is a
    to_string function for the type of elements in t *)

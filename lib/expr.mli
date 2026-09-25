type t

val num : float -> t
val var : string -> t
val zero : t
val one : t

val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t
val div : t -> t -> t
val max : t -> t -> t
val log : t -> t        (* Base 2 *)

val to_string : t -> string

(* List of var id + var value *)
type env = (string * float) list

val eval : env -> t -> float

(* Returns list of all var ids present *)
val vars : t -> string list


open Tensor
type 'a tensor = 'a Tensor.tensor

type ('a,'b) t

val atom  : string -> ('a -> 'b) -> ('a, 'b) t
val (>>>) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
val (|||) : ('a, 'b) t -> ('c, 'd) t -> ('a * 'c, 'b * 'd) t
val par : int list -> ('a, 'b) t -> ('a tensor, 'b tensor) t
val extend : int list -> ('a, 'b) t -> ('a, 'b tensor) t
val reduce : int list -> ('a * 'a, 'a) t -> ('a tensor, 'a) t

(* Cost tuple + parameters *)
type cost = { 
  lat   : Expr.t; 
  i_sz  : Expr.t;
  o_sz  : Expr.t;
  mem   : Expr.t;
  new_params : Expr.t list 
}

val get_id : ('a, 'b) t -> int
val get_cost : ('a, 'b) t -> cost
val to_string : ('a, 'b) t -> string 

val zeta : ('a, 'b) t -> ('a * 'b, 'a) t
val dump_vars : unit -> (string * Expr.t list) Seq.t

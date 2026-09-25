(* Shape *)
type shape = int list

val numel : shape -> int
val string_of_shape : shape -> string

(* Tensor *)
type 'a tensor = {
  data  : 'a list  ;
  shape : shape
}

(* Initializes a tensor *)
val init   : shape -> 'a -> 'a tensor

(* Pairs in index order *)
val zip    : 'a tensor -> 'b tensor -> ('a * 'b) tensor 

(* Zip inverse *)
val unzip  : ('a * 'b) tensor -> 'a tensor * 'b tensor 

(* All pairs of combinations *)
val prod   : 'a tensor -> 'b tensor -> ('a * 'b) tensor 

(* Splits a tensor in the first n axes *)
val split  : 'a tensor -> int -> 'a tensor tensor

(* Inverse of split *)
val merge  : 'a tensor tensor -> 'a tensor

(* Concatenates two tensors of the same shape *)
val cat    : int -> 'a tensor -> 'a tensor -> 'a tensor

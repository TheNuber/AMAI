type t =
  | Num of float
  | Var of string
  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Div of t * t
  | Max of t * t                 
  | Log of t

let num x = Num x
let var s = Var s
let zero  = Num 0.
let one   = Num 1.

let add a b = match a, b with
  | Num 0., x | x, Num 0. -> x
  | Num a, Num b          -> Num (a +. b)
  | _                     -> Add (a, b)

let sub a b = match a, b with
  | x, Num 0. -> x
  | Num a, Num b          -> Num (a -. b)
  | _                     -> Sub (a, b)

let mul a b = match a, b with
  | Num 0., _ | _, Num 0. -> Num 0.
  | Num 1., x | x, Num 1. -> x
  | Num a, Num b          -> Num (a *. b)
  | _                     -> Mul (a, b)

let div a b = match a, b with
  | Num 0., _              -> Num 0.
  | x, Num 1.             -> x
  | Num a, Num b when b <> 0. -> Num (a /. b)
  | _                     -> Div (a, b)

let max a b = match a, b with
  | Num a, Num b -> Num (Stdlib.max a b)
  | _            -> Max (a, b)

(* Base 2 *)
let _log2 (x : float) = (Stdlib.log x) /. (Stdlib.log 2.)
let log a = match a with
  | Num x -> Num (_log2 x) 
  | _     -> Log a

let rec to_string = function
  | Num x       -> Printf.sprintf "%g" x
  | Var s       -> s
  | Add (a, b)  -> "(" ^ to_string a ^ " + " ^ to_string b ^ ")"
  | Sub (a, b)  -> "(" ^ to_string a ^ " - " ^ to_string b ^ ")"
  | Mul (a, b)  -> paren a ^ "*" ^ paren b
  | Div (a, b)  -> paren a ^ "/" ^ paren b
  | Max (a, b)  -> "max(" ^ to_string a ^ ", " ^ to_string b ^ ")"
  | Log a       -> "log(" ^ to_string a ^ ")"
and paren e = match e with
  | Num _ | Var _ -> to_string e
  | _             -> "(" ^ to_string e ^ ")"

(* List of var id + var value *)
type env = (string * float) list

let rec eval (e : env) = function
  | Num x       -> x
  | Var s       -> (try List.assoc s e with Not_found -> nan)
  | Add (a, b)  -> eval e a +. eval e b
  | Sub (a, b)  -> eval e a -. eval e b
  | Mul (a, b)  -> eval e a *. eval e b
  | Div (a, b)  -> eval e a /. eval e b
  | Max (a, b)  -> Stdlib.max (eval e a) (eval e b)
  | Log a       -> _log2 (eval e a)

module Var_set = Set.Make (String)
 
let vars e =
  let rec go acc = function
    | Num _                                -> acc
    | Var s                                -> Var_set.add s acc
    | Log a                                -> go acc a
    | Add (a, b) | Sub (a, b) | Mul (a, b)
    | Div (a, b) | Max (a, b)              -> go (go acc a) b
  in
  Var_set.elements (go Var_set.empty e)



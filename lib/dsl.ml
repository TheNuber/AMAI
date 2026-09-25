open Expr
open Tensor

type 'a tensor = 'a Tensor.tensor


(* Mirror of the interface, it provides the structural data *)
type ('a, 'b) flow = 
  | Atom   : string * ('a -> 'b) -> ('a, 'b) flow                          (* atom         *)
  | Seq    : ('a, 'b) flow * ('b, 'c) flow -> ('a, 'c) flow            (* p >>> q      *)
  | Conc   : ('a, 'b) flow * ('c, 'd) flow -> ('a * 'c, 'b * 'd) flow  (* p ||| q      *)
  | Par    : int list * ('a, 'b) flow -> ('a tensor, 'b tensor) flow     (* par [.] p    *)
  | Extend : int list * ('a, 'b) flow -> ('a, 'b tensor) flow            (* extend [.] p *)
  | Reduce : int list * ('a * 'a, 'a) flow -> ('a tensor, 'a) flow       (* reduce [.] q *)

(* Cost tuple + parameters involved *)
type cost = { 
  lat   : Expr.t; 
  i_sz  : Expr.t;
  o_sz  : Expr.t;
  mem   : Expr.t;
  new_params : Expr.t list 
}

(* We keep an id as hash value for memoization *)
type ('a, 'b) t = {
  id    : int; 
  flow  : ('a, 'b) flow; 
  cost  : cost 
}

let get_id (x : ('a, 'b) t) = x.id
let get_cost (x : ('a, 'b) t) = x.cost


let rec string_of_flow : type a b. (a, b) flow -> string = function
  | Atom (s, f)     -> s
  | Seq (p, q)     -> string_of_flow p ^ " >>> " ^ (string_of_flow q)
  | Conc (p, q)    -> string_of_flow p ^ " ||| " ^ (string_of_flow q)
  | Par (sh, p)    -> "Par " ^ string_of_shape sh ^ " (" ^ (string_of_flow p) ^ ")"
  | Extend (sh, p) -> "Extend " ^ string_of_shape sh ^ " (" ^ (string_of_flow p) ^ ")"
  | Reduce (sh, p) -> "Reduce " ^ string_of_shape sh ^ " (" ^ (string_of_flow p) ^ ")"


let to_string (p : ('a, 'b) t) = string_of_flow p.flow


(* Alias for working with expressions using more compact code *)
let v = Expr.var 
and ( *% ) = Expr.mul 
and ( +% ) = Expr.add 
and ( /% ) = Expr.div
and ( ^% ) = Expr.max


let param_ctr = ref 0
let fresh_params specs =
  incr param_ctr; let pid = !param_ctr in
  List.map (fun sfx -> Expr.var (Printf.sprintf "p%d_%s" pid sfx)) specs


(* Implementation of the DSL mapping *)


(* Memoization of constructed expressions with hash tables *)
let htb_size = 97
let atom_memo   : (string, int * cost) Hashtbl.t         = Hashtbl.create htb_size
let seq_memo    : (int * int, int * cost) Hashtbl.t      = Hashtbl.create htb_size
let conc_memo   : (int * int, int * cost) Hashtbl.t      = Hashtbl.create htb_size
let par_memo    : (shape * int, int * cost) Hashtbl.t = Hashtbl.create htb_size
let extend_memo : (shape * int, int * cost) Hashtbl.t = Hashtbl.create htb_size
let reduce_memo : (shape * int, int * cost) Hashtbl.t = Hashtbl.create htb_size

let cached tbl key compute =
  match Hashtbl.find_opt tbl key with
  | Some c -> c
  | None -> let c = compute () in Hashtbl.add tbl key c; c


let next_id = let c = ref 0 in fun () -> incr c; !c


(* ARR *)
let atom : type a b. string -> (a -> b) -> (a, b) t = 
  fun (s_id : string) (f : a -> b) ->
  let id, cost = cached atom_memo s_id (fun () -> next_id (),
    match fresh_params [ "l"; "i"; "o"; "m" ] with
    | [l;i;o;m] -> { 
        lat = l; 
        i_sz = i; 
        o_sz = o; 
        mem = m; 
        new_params = [l;i;o;m] 
      }
    | _ -> assert false)
  in { id; flow = Atom (s_id, f); cost }


(* >>> *)
let seq_cost a b = { 
  lat  = a.lat +% b.lat;
  i_sz = a.i_sz;
  o_sz = b.o_sz;
  mem  = a.mem ^% b.mem;
  new_params = [] 
}

let ( >>> ) (p : ('a,'b) t) (q : ('b,'c) t) : ('a,'c) t =
  let id, cost = cached seq_memo (p.id, q.id) (fun () -> next_id (), seq_cost p.cost q.cost) in
  { id; flow = Seq (p.flow, q.flow); cost }


(* ||| *)
let conc_cost a b = {
  lat  = a.lat ^% b.lat;
  i_sz = a.i_sz +% b.i_sz;
  o_sz = a.o_sz +% b.o_sz;
  mem  = a.mem +% b.mem;
  new_params = [] 
}

let ( ||| ) (p : ('a,'b) t) (q : ('c,'d) t) : ('a * 'c, 'b * 'd) t =
  let id, cost = cached conc_memo (p.id, q.id) (fun () -> next_id (), conc_cost p.cost q.cost) in
  { id; flow = Conc (p.flow, q.flow); cost }


(* PAR *)
let par (sh : int list) (p : ('a,'b) t) : ('a tensor, 'b tensor) t =
  let n = Expr.num (float_of_int (List.fold_left ( * ) 1 sh)) in
  let id, cost = cached par_memo (sh, p.id) (fun () -> next_id (),
    match fresh_params [ "P" ] with
    | [n_P] -> { 
        lat  = (n /% n_P) *% p.cost.lat;
        i_sz = (n /% n_P) *% p.cost.i_sz;
        o_sz = (n /% n_P) *% p.cost.o_sz;
        mem  = (n /% n_P) *% p.cost.mem;
        new_params = [n_P] 
      }
    | _ -> assert false)
  in
  { id; flow = Par (sh, p.flow); cost }


(* EXTEND *)
let extend (sh : int list) (p : ('a,'b) t) : ('a, 'b tensor) t =
  let n = Expr.num (float_of_int (List.fold_left ( * ) 1 sh)) in
  let id, cost = cached extend_memo (sh, p.id) (fun () -> next_id (),
    match fresh_params [ "P"; "alpha"; "beta" ] with
    | [n_P; a; b] -> { 
        lat  = (a +% (b *% p.cost.i_sz)) *% (log n_P) +% (n /% n_P) *% p.cost.lat; 
        i_sz = p.cost.i_sz;
        o_sz = (n /% n_P) *% p.cost.o_sz;
        mem  = p.cost.i_sz ^% p.cost.mem;
        new_params = [n_P;a;b] 
      }
    | _ -> assert false)
  in
  { id; flow = Extend (sh, p.flow); cost }


(* REDUCE *)
let reduce (sh : int list) (p : ('a * 'a, 'a) t) : ('a tensor, 'a) t =
  let n = Expr.num (float_of_int (List.fold_left ( * ) 1 sh)) in
  let id, cost = cached reduce_memo (sh, p.id) (fun () -> next_id (),
    match fresh_params [ "P"; "alpha"; "beta" ] with
    | [n_P; a; b] -> { 
        lat  = (p.cost.lat +% a +% (b *% p.cost.o_sz)) *% (log n_P) +% (n /% n_P) *% p.cost.lat; 
        i_sz = (n /% n_P) *% (p.cost.i_sz /% num 2.);
        o_sz = p.cost.o_sz;
        mem  = ((n /% n_P) *% p.cost.mem) ^% p.cost.o_sz; 
        new_params = [n_P;a;b] 
      }
    | _ -> assert false)
  in
  { id; flow = Reduce (sh, p.flow); cost }


let rec build : type a b. (a, b) flow -> (a, b) t = function
  | Atom (s, f)     -> atom s f
  | Seq (p, q)     -> (build p) >>> (build q)
  | Conc (p, q)    -> (build p) ||| (build q)
  | Par (sh, p)    -> par sh (build p)
  | Extend (sh, p) -> extend sh (build p)
  | Reduce (sh, p) -> reduce sh (build p)


let rec zeta_flow : type a b. (a, b) flow -> (a * b, a) t = fun nn ->
  let id  () = atom "id"  (fun x -> x)
  and fst () = atom "fst" (fun (x,y) -> x)
  and snd () = atom "snd" (fun (x,y) -> y)
  and dup () = atom "dup" (fun x -> (x,x))
  in match nn with
  | Atom (s, f) ->
      fst ()
      >>> dup ()
      >>> ((atom s f) ||| id ())
      >>> snd ()
  | Seq (p, q) ->
      dup ()
      >>> (fst () ||| id () )
      >>> (id ()  ||| ((build p ||| id ()) >>> zeta_flow q))
      >>> zeta_flow p
  | Conc (p, q) ->
      atom "reassoc_conc" (fun ((a, c), (b, d)) -> ((a, b), (c, d)))
      >>> (zeta_flow p ||| zeta_flow q)
  | Par (sh, p) -> atom "zip" (fun (x,y) -> zip x y) >>> par sh (zeta_flow p)
  | Extend (sh, p) -> 
      (id () ||| reduce sh (atom "bwd_red" (fun (x,_) -> x)))
      >>> zeta_flow p
  | Reduce (sh, q) -> 
      snd ()
      >>> extend sh (id ())

(* Specification that approximates a forward-backward pass *)
let zeta (x : ('a, 'b) t) : ('a * 'b, 'a) t = zeta_flow x.flow

let dump_vars : unit -> (string * Expr.t list) Seq.t = fun _ ->
  let atom_seq = Hashtbl.to_seq atom_memo in
  let atom_seq_transf = Seq.map (fun (x,(i,y)) -> (x, y.new_params)) atom_seq in
  let shape_transf = fun x -> 
    let seq = Hashtbl.to_seq x in 
    Seq.map (fun ((s,j),(i,y)) -> (string_of_shape s, y.new_params)) seq in
  Seq.append atom_seq_transf 
  (Seq.append (shape_transf par_memo) (
  Seq.append (shape_transf extend_memo) (shape_transf reduce_memo)))

(* Shape *)

type shape = int list

let numel = List.fold_left ( * ) 1

let string_of_shape (sh : shape) = 
  let hd, tl = List.hd sh, List.tl sh in
  let stl = List.map string_of_int tl in
  let rstl = List.fold_left (fun x y -> x ^ "," ^ y) "" stl in
  "[" ^ (string_of_int hd) ^ rstl ^ "]"

(* Tensor *)

type 'a tensor = {
  data  : 'a list  ;
  shape : shape
}

let init (sh : shape) x = {
  data = List.init (numel sh) (fun _ -> x) ;
  shape = sh
}

let zip (v : 'a tensor) (w : 'b tensor) = {
  data = List.combine v.data w.data;
  shape = v.shape
}
    
let unzip (vw : ('a * 'b) tensor) = 
  let v_data, w_data = List.split vw.data in
  let v = { data = v_data; shape = vw.shape }
  and w = { data = w_data; shape = vw.shape }
  in v, w

let prod (v : 'a tensor) (w: 'b tensor) = 
  let vw_shape = v.shape @ w.shape in
  let vw_prods = 
    List.map (fun x -> 
    List.map (fun y -> (x, y) ) w.data ) v.data in
  { data = List.concat vw_prods ; shape = vw_shape }
  
let split (v : 'a tensor) (n : int) = 
  let tensor_shape = List.take n v.shape in
  let subtensor_shape = List.drop n v.shape in
  let chunk_size = numel subtensor_shape in
  let rec chunk acc lst =
    match lst with
    | [] -> List.rev acc
    | _ -> let ch = List.take chunk_size lst in
           chunk (ch :: acc) (List.drop chunk_size lst)
  in 
  let chunks = chunk [] v.data in { 
    data  = List.map (fun x -> { data = x; shape = subtensor_shape }) chunks ;
    shape = tensor_shape
  }

let merge (vt : 'a tensor tensor) = 
  let v_data = vt.data |> List.map (fun x -> x.data) |> List.concat in
  let v_shape = vt.shape @ (List.hd vt.data).shape in
  { data = v_data ; shape = v_shape }

exception ShapesDoNotMatch
let cat (dim : int) (v : 'a tensor) (w: 'a tensor) = 
  let vpre, wpre = List.take dim v.shape, List.take dim w.shape in
  let vd, wd = List.nth v.shape dim, List.nth w.shape dim in
  let vsuf, wsuf = List.drop (dim+1) v.shape, List.drop (dim+1) w.shape in
  if vpre <> wpre || vsuf <> wsuf then raise ShapesDoNotMatch else
  { data  = v.data @ w.data ; shape = vpre @ [vd+wd] @ vsuf }

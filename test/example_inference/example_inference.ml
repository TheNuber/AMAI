open AMAI.Dsl
open AMAI.Expr
open AMAI.Tensor

(* Small components *)

let id () = atom "id" (fun x -> x)
let dup () = atom "dup" (fun x -> (x,x))
let triple = atom "triple" (fun x -> ((x,x),x))

let add = atom "add" (fun (x,y) -> x +. y)
let mul = atom "mul" (fun (x,y) -> x *. y)
let square = atom "square" (fun x -> x *. x)
let silu = atom "silu" (fun x -> x *. (1. /. (1. +. exp (-. x))))

let load = atom "load" (fun x -> 1.)


(* Tensor helpers *) 
let zip () = atom "zip" (fun (x,y) -> AMAI.Tensor.zip x y)
let prod () = atom "prod" (fun (x,y) -> AMAI.Tensor.prod x y)
let split n = atom "split" (fun x -> AMAI.Tensor.split x n)
let merge () = atom "merge" AMAI.Tensor.merge
let cat = atom "cat" (fun (x,y) -> AMAI.Tensor.cat 1 x y)

(* Linear layer *)
let dot k = 
  par [k] (dup () >>> (load ||| id ()) >>> mul)   
  >>> reduce [k] add                       
let linear k n = extend [n] (dot k)

(* Matmul *)
let dotprod k = zip () 
            >>> par [k] mul
            >>> reduce [k] add
let matmul n m k = 
      (split 1 ||| split 1)
  >>> prod ()
  >>> par [n;m] (dotprod k)

let hidden_size = 4096

(* Norm *) 
let eps = 1e-6
let rms n = par [n] square 
        >>> reduce [n] add
        >>> atom "scale_rms" (fun x -> 1. /. (sqrt (x /. (float_of_int n) +. eps)))

let rmsnorm n = dup ()
            >>> (id () ||| (rms n >>> extend [1] (id ())))
            >>> prod ()
            >>> par [n] mul
            >>> linear 1 n

let norm = rmsnorm hidden_size


(* Attention *)

let dim = 512

let linearQ = linear hidden_size dim
let linearK = linear hidden_size dim
let linearV = linear hidden_size dim
let linearO = linear hidden_size dim

let core_att =  
    let qk = matmul hidden_size hidden_size dim
         >>> split 1
         >>> par [hidden_size] (par [hidden_size] (atom "softmax" (fun x -> x)))
         >>> merge () 
    and xv = matmul hidden_size hidden_size dim in
    (qk ||| id ()) >>> xv

let attention = triple 
                >>> ((linearQ ||| linearK) ||| linearV) 
                >>> core_att 
                >>> linearO
                
(* FFN *)

let inter_size = 8192

let vsilu = par [inter_size] silu
let vmul = zip () 
       >>> par [inter_size] mul

let linearW1 = linear inter_size hidden_size
let linearW2 = linear inter_size hidden_size
let linearW3 = linear hidden_size inter_size

let ffn = dup ()
          >>> ((linearW1 >>> vsilu) ||| linearW2)
          >>> vmul 
          >>> linearW3

(* Layer *)
let residual = zip () >>> par [hidden_size; dim] add

let vocab_size = 128000
let embed_in = linear hidden_size vocab_size
let embed_out = linear vocab_size hidden_size

let layer = dup () >>> ((norm >>> attention) ||| id ()) >>> residual
        >>> dup () >>> ((norm >>> ffn      ) ||| id ()) >>> residual

let l = 24  (* Number of layers *)
let stack = let layers = List.init l (fun _ -> layer) in
            List.fold_left (>>>) (id ()) layers

let model = embed_in >>> stack >>> embed_out

let b = 1  (* Batch size *)
let dp = 4 (* Data Parallel degree *)
let batched = par [b] model
let full = par [dp] batched >>> reduce [dp] cat


(* Main *)

let parse_args () =
  let args = Array.to_list Sys.argv |> List.tl in
  match args with
  | ["generate"] -> "generate"
  | ["analyze"] -> "analyze"
  | _ -> print_endline "Bad argument: only 'generate' and 'analyze' are supported";
         exit 1;

open AMAI.Serialize

let () = let mode = parse_args () in
  let cost = get_cost model in
  print_endline "Transformer-based Model Inference Example";
  if mode = "generate" then 
  begin
    write_env_template "./env_template.csv" cost;
    let oc = open_out "model.txt" in
    output_string oc (Printf.sprintf "lat  = %s\n" (AMAI.Expr.to_string cost.lat));
    output_string oc (Printf.sprintf "i_sz = %s\n" (AMAI.Expr.to_string cost.i_sz));
    output_string oc (Printf.sprintf "o_sz = %s\n" (AMAI.Expr.to_string cost.o_sz));
    output_string oc (Printf.sprintf "mem  = %s\n" (AMAI.Expr.to_string cost.mem))
  end
  else
    let e = read_env "./env_template.csv" in
    let l, i, o, m = eval_cost e cost in
    let oc = open_out "final_values.txt" in
    output_string oc (Printf.sprintf "lat  = %f\n" l);
    output_string oc (Printf.sprintf "i_sz = %f\n" i);
    output_string oc (Printf.sprintf "o_sz = %f\n" o);
    output_string oc (Printf.sprintf "mem  = %f\n" m)

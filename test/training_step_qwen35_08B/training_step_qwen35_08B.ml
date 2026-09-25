open AMAI.Dsl

let eps       = 1e-6

(* Generic combinators *)
let id ()     = atom "id" (fun x -> x)
let dup ()    = atom "dup" (fun x -> (x, x))
let dup3 ()   = atom "dup3" (fun x -> (x, (x, x)))
let dup4 ()   = atom "dup4" (fun x -> (x, (x, (x, x))))
let first p   = p ||| id ()
let second p  = id () ||| p

(* Atoms for scalar operations *)
let add       = atom "add" (fun (x, y) -> x +. y)
let mul       = atom "mul" (fun (x, y) -> x *. y)
let square    = atom "square" (fun x -> x *. x)
let sub       = atom "sub" (fun (x, y) -> x -. y)
let recip     = atom "recip" (fun x -> 1. /. x)
let expf      = atom "expf" (fun x -> exp x)
let sq        = atom "sq" (fun x -> x *. x)
let sigmoid   = atom "sigmoid" (fun x -> 1. /. (1. +. exp (-. x)))
let silu      = atom "silu" (fun x -> x *. (1. /. (1. +. exp (-. x))))
let softplus  = atom "softplus" (fun x -> Stdlib.log (1. +. exp x))
let scale_rms n = atom "scale_rms" (fun x -> 1. /. (sqrt (x /. (float_of_int n) +. eps)))



(* Data load and movement *)
let init_weight = fun _ -> 0.
let load ()     = atom "load" (fun _ -> init_weight ())
let window () = atom "window" (fun x -> x)
let rope ()   = atom "rope" (fun v -> v)
let causal () = atom "causal" (fun v -> v)


(* Tensor helpers *)
let zip ()   = atom "zip" (fun (x,y) -> AMAI.Tensor.zip x y)
let unzip () = atom "unzip" (AMAI.Tensor.unzip)
let prod ()  = atom "prod" (fun (x,y) -> AMAI.Tensor.prod x y)
let split n  = atom "split" (fun x -> AMAI.Tensor.split x n)
let merge () = atom "merge" (AMAI.Tensor.merge)
let cat ()   = atom "cat" (fun (x,y) -> AMAI.Tensor.cat 1 x y)
 
let vadd n = zip () >>> par [n] add
let vmul n = zip () >>> par [n] mul
let broadcast n = extend [n] (id ())


(* ===== MATMUL & LINEAR ===== *)

(* Linear layer *)
let dot k = 
  par [k] (dup () >>> (load () ||| id ()) >>> mul)   
  >>> reduce [k] add                       
let linear k n = extend [n] (dot k)

(* Matmul *)
let dotprod k = vmul k
            >>> reduce [k] add
let matmul n m k = 
      (split 1 ||| split 1)
  >>> prod ()
  >>> par [n;m] (dotprod k)


(* ===== CONFIG (Qwen3.5-0.8B-Base dense) ===== *)
let h        = 1024
let inter    = 3584
let vocab    = 248320
let n_layer  = 24

let n_head   = 8
let n_kv     = 2
let d_head   = 256
let g_grp    = n_head / n_kv
let rope_dim = d_head / 4

let lk = 16  and lv = 16
let dk = 128 and dv = 128
let conv_k   = 4
let key_dim  = dk * lk
let val_dim  = dv * lv
let conv_dim = key_dim * 2 + val_dim

(* RMSNorm *)
let eps = 1e-6
let rms n = par [n] square 
        >>> reduce [n] add
        >>> scale_rms n

let rmsnorm n = dup ()
            >>> (id () ||| (extend [1] (rms n)))
            >>> prod ()
            >>> par [n] mul
            >>> linear 1 n

let rmsnorm_gated n = (rmsnorm n ||| par [n] silu)
                  >>> vmul n


(* Full Attention *)

let q_block = dup ()
  >>> ((linear h (n_head * d_head)) ||| (linear h (n_head * d_head)))
  >>> first (split 1 
         >>> par [n_head] (rmsnorm d_head >>> rope ())
         >>> merge ())

let k_block =
  linear h (n_kv * d_head) >>> split 1
  >>> par [n_kv] (rmsnorm d_head >>> rope ())
  >>> merge ()
  >>> broadcast g_grp
  >>> merge ()

let v_block =
  linear h (n_kv * d_head)
  >>> broadcast g_grp
  >>> merge ()

let proj_qkv s = dup3 () 
  >>> ((par [s] q_block >>> unzip ()) ||| (par [s] k_block ||| par [s] v_block))

let softmax_atom s =
  par [s] expf
  >>> dup () 
  >>> second (extend [1] (reduce [s] add >>> recip))
  >>> prod ()
  >>> par [s] mul

let sqd = sqrt (float_of_int d_head)

let qk_softmax s =
  par [n_head] (matmul s s d_head
            >>> par [s;s] (causal () >>> (atom "scale_isq" (fun x -> x /. sqd)))
            >>> split 1
            >>> par [s] (softmax_atom s)
            >>> merge ())

let core_attention s =
  first (zip () >>> qk_softmax s)
  >>> zip () >>> par [n_head] (matmul s d_head s)

let gate_out s =
  par [n_head] (par [s;d_head] sigmoid)

let full_attn s =
  proj_qkv s
  >>> atom "att_reshape" (fun ((q, g), (k, v)) -> (g, ((q, k), v)))
  >>> (gate_out s ||| core_attention s)
  >>> zip ()
  >>> par [s] (vmul (n_head * d_head))
  >>> par [s] (linear (n_head * d_head) h)

(* Linear attention *)
let linearQKV = linear h conv_dim   (* Beware: model for Q or K or V *)
let linearZ = linear h val_dim
let linearA = linear h lv
let linearB = linear h lv

let kernel1d k = extend [k] (dup () 
                        >>> (window () ||| load ()) 
                        >>> mul)
let conv1d_causal =
  par [3*conv_dim] (kernel1d conv_k >>> reduce [conv_k] add)

let preprocessQKV = extend [3] (
          linearQKV 
          >>> conv1d_causal 
          >>> par [conv_dim] silu
          >>> rmsnorm conv_dim
        ) 

let decay =
  dup () >>> (id () ||| load ()) >>> add
  >>> softplus
  >>> dup () >>> second (load () >>> expf) >>> mul 
  >>> atom "neg" (fun x -> -. x)

(* Important: delta rule is modelled atomically, no more detail of it*)
let delta_rule = atom "delta_rule" (fun (qkv, (a,b)) -> qkv)

let linear_attn s = par [s] (dup ()
  >>> (
    (dup () >>> (
        preprocessQKV 
        ||| 
        (dup () >>> ((par [h;lv] decay) ||| (par [h;lv] sigmoid)))
      ) >>> delta_rule >>> merge ()
    )
    |||
    linearZ
  ) >>> rmsnorm_gated val_dim
)

(* MLP *)

let linearW1 = linear inter h
let linearW2 = linear inter h
let linearW3 = linear h inter

let ffn = dup ()
          >>> ((linearW1 >>> par [inter] silu) ||| linearW2)
          >>> vmul inter
          >>> linearW3


(* Layer and full model *)
let seq_add s n = zip () >>> par [s] (vadd n)

let layer s i =
  let mixer = if (i + 1) mod 4 = 0 then full_attn s else linear_attn s in
      dup () >>> second (par [s] (rmsnorm h) >>> mixer) >>> seq_add s h
  >>> dup () >>> second (par [s] (rmsnorm h) >>> par [s] ffn) >>> seq_add s h

let stack s = let layers = List.init n_layer (fun i -> layer s i) in
            List.fold_left (>>>) (id ()) layers

let embed_in = linear h vocab
let embed_out = linear vocab h

let decoder s =
  par [s] (embed_in)
  >>> stack s
  >>> par [s] (rmsnorm h)
  >>> par [s] (embed_out)

(* Global view *)
let batch_size = 4
let data_parallel = 32

(* Key point: forward + backward pass analyzed recursively through an approximation *)
let train_step s = par [data_parallel] (zeta (par [batch_size] (decoder s)))
               >>> reduce [data_parallel] (cat ())


let model = train_step 4096
(* MAIN *)

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
  print_endline "Qwen3.5-Dense Training Step";
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

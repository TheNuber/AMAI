(* ---- 1. structure CSV: id, flow, new_params ---- *)
val write_structure_csv : string -> ('a, 'b) Dsl.t -> unit

(* ---- 2. dump CSV: string, list of strings *)
val write_dump_template : string -> unit

(* ---- 3. params CSV: variable, value ---- *)
val write_env_template : ?default:float -> string -> Dsl.cost -> unit
val read_env : string -> Expr.env
val eval_cost : Expr.env -> Dsl.cost -> float * float * float * float

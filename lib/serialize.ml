(* ---- minimal RFC-4180-style CSV field escaping / parsing ---- *)

let escape_field (s : string) : string =
  if String.exists (fun c -> c = ',' || c = '"' || c = '\n' || c = '\r') s
  then begin
    let buf = Buffer.create (String.length s + 2) in
    Buffer.add_char buf '"';
    String.iter
      (fun c -> if c = '"' then Buffer.add_string buf "\"\"" else Buffer.add_char buf c)
      s;
    Buffer.add_char buf '"';
    Buffer.contents buf
  end
  else s

(* Joins a CSV line *)
let csv_line (fields : string list) : string =
  String.concat "," (List.map escape_field fields)

(* Splits a CSV line *)
let split_csv_line (line : string) : string list =
  let n = String.length line in
  let fields = ref [] in
  let buf = Buffer.create 16 in
  let in_quotes = ref false in
  let i = ref 0 in
  while !i < n do
    let c = line.[!i] in
    (if !in_quotes then
       if c = '"' then
         if !i + 1 < n && line.[!i + 1] = '"' then begin
           Buffer.add_char buf '"';
           incr i
         end
         else in_quotes := false
       else Buffer.add_char buf c
     else if c = '"' then in_quotes := true
     else if c = ',' then begin
       fields := Buffer.contents buf :: !fields;
       Buffer.clear buf
     end
     else Buffer.add_char buf c);
    incr i
  done;
  fields := Buffer.contents buf :: !fields;
  List.rev !fields

(* ---------------------------------------------------------------- *)
(* 1. structure CSV: id, flow, new_params                            *)
(* ---------------------------------------------------------------- *)

open Dsl

let new_params_field (c : cost) : string =
  String.concat ";" (List.map Expr.to_string c.new_params)

let structure_row (x : ('a, 'b) t) : string =
  let cost = get_cost x in
  csv_line
    [ string_of_int (get_id x); to_string x; new_params_field cost ]

let structure_header = "id,flow,new_params"

let write_structure_csv (path : string) (x : ('a, 'b) t) : unit =
  let oc = open_out path in
  output_string oc (structure_header ^ "\n");
  output_string oc (structure_row x ^ "\n");
  close_out oc


(* ---------------------------------------------------------------- *)
(* 2. Dump related vars on a CSV                                    *)
(* ---------------------------------------------------------------- *)

let dump_header = "flow,vars"

let string_of_varlist vl = 
  let hd, tl = List.hd vl, List.tl vl in
  let stl = List.map Expr.to_string tl in
  let rstl = List.fold_left (fun x y -> x ^ "," ^ y) "" stl in
  "[" ^ (Expr.to_string hd) ^ rstl ^ "]"

let write_dump_template (path : string) : unit =
  let oc = open_out path in
  output_string oc (dump_header ^ "\n");
  Seq.iter
    (fun (s,vl) -> output_string oc (csv_line [ s; string_of_varlist vl ] ^ "\n"))
    (dump_vars ());
  close_out oc




(* ---------------------------------------------------------------- *)
(* 3. params CSV: variable, value                                   *)
(* ---------------------------------------------------------------- *)

module SS = Set.Make (String)

let params_header = "name,value"

let gen_env_template ?(default = 0.0) (c : cost) : Expr.env =
  List.map (fun e -> SS.of_list (Expr.vars e)) [c.lat; c.i_sz; c.o_sz; c.mem]
  |> List.fold_left (fun x y -> SS.union x y) SS.empty
  |> SS.elements
  |> List.map (fun x -> (x, default))

let write_env_template ?(default = 0.0) (path:string) (c:cost) =
  let oc = open_out path in
  output_string oc (params_header ^ "\n");
  (gen_env_template c)
  |> List.iter (fun (name,value) -> 
      output_string oc (csv_line [ name; Printf.sprintf "%.1f" value ] ^ "\n"));
  close_out oc

let read_env (path : string) : Expr.env =
  let ic = open_in path in
  let rec loop acc first =
    match input_line ic with
    | exception End_of_file -> List.rev acc
    | line ->
      if first then loop acc false (* skip header row *)
      else match split_csv_line line with
        | [ name; value ] -> loop ((name, float_of_string value) :: acc) false
        | _ ->               loop acc false (* skip blank / malformed lines *)
  in
  let result = loop [] true in
  close_in ic;
  result

let eval_cost (e : Expr.env) (c : cost) = ( 
  Expr.eval e c.lat, 
  Expr.eval e c.i_sz, 
  Expr.eval e c.o_sz, 
  Expr.eval e c.mem
)

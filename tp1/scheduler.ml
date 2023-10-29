open Netlist_ast
open Graph

exception Combinational_cycle

let read_exp eq option =
  let auxarg a acc = match a with
    | Avar i -> i::acc
    | _ -> acc in
  let rec aux eq  acc = match eq with
    | Earg a | Enot a -> auxarg a acc
    | Ereg i -> if(option)then i::acc else acc
    | Ebinop (_,a,b) -> auxarg a (auxarg b acc)
    | Erom (_,_,a) -> auxarg a acc
    | Eram (_,_,a,b,c,d) when option-> auxarg a (auxarg b (auxarg c (auxarg d acc)))
    | Eram (_,_,a,_,_,_) -> auxarg a acc
    | Econcat (a,b) -> auxarg a (auxarg b acc)
    | Eslice (_,_,a) -> auxarg a acc
    | Eselect (_,a) -> auxarg a acc 
    | Emux(a,b,c) -> auxarg a (auxarg b (auxarg c acc)) in
  let i,expr = eq in
  aux expr [] 
  



let program_to_graph p =
  let g = mk_graph() in
  let rec add_exp l = match l with
    | [] -> ()
    | eq::q -> (add_node_list g (read_exp eq true);add_exp q) in
  add_exp p.p_eqs;
  add_node_list g p.p_inputs;
  add_node_list g p.p_outputs;
  let rec add_eq l = match l with
    | [] -> ()
    | (i,expr)::q -> (add_edge_list g i (read_exp (i,expr) false);add_eq q) in
  add_eq p.p_eqs;g



let rec variables_to_eqs p_eqs l = match l with
  | [] -> []
  | v::q -> if(List.exists (fun (a,b) -> a = v) p_eqs)then List.find (fun (a,b) -> a = v) p_eqs :: variables_to_eqs p_eqs q else variables_to_eqs p_eqs q


let schedule p = 
  let g = program_to_graph p in
  if (has_cycle g)then raise Combinational_cycle;
  let order = topological g in
  let eqlist = variables_to_eqs p.p_eqs order in {p_eqs = eqlist; p_inputs = p.p_inputs; p_outputs = p.p_outputs; p_vars = p.p_vars}






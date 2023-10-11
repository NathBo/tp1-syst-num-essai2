exception Cycle
type mark = NotVisited | InProgress | Visited


type 'a graph =
    { mutable g_nodes : 'a node list }
and 'a node = {
  n_label : 'a;
  mutable n_mark : mark;
  mutable n_link_to : 'a node list;
  mutable n_linked_by : 'a node list;
}

let mk_graph () = { g_nodes = [] }


let is_a_node g x =
  List.exists (fun n -> n.n_label = x) g.g_nodes


let add_node g x =
  let n = { n_label = x; n_mark = NotVisited; n_link_to = []; n_linked_by = [] } in
  g.g_nodes <- n :: g.g_nodes

let rec add_node_list g l = match l with
  | x::q -> (if (not(is_a_node g x)) then add_node g x;add_node_list g q)
  | [] -> ()

let node_of_label g x =
  List.find (fun n -> n.n_label = x) g.g_nodes

let add_edge g id1 id2 =
  try
    let n1 = node_of_label g id1 in
    let n2 = node_of_label g id2 in
    n1.n_link_to   <- n2 :: n1.n_link_to;
    n2.n_linked_by <- n1 :: n2.n_linked_by
  with Not_found -> Format.eprintf "Tried to add an edge between non-existing nodes"; raise Not_found

let rec add_edge_list g a l = match l with
  | [] -> ()
  | x::q -> (add_edge g x a;add_edge_list g a q)


let clear_marks g =
  List.iter (fun n -> n.n_mark <- NotVisited) g.g_nodes

let find_roots g =
  List.filter (fun n -> n.n_linked_by = []) g.g_nodes

let has_cycle g =
    let rec dfs l = match l with
      | [] -> false
      | x::q -> match x.n_mark with
        | NotVisited -> (x.n_mark <- InProgress;
          let rep = dfs x.n_link_to in
          x.n_mark <- Visited;rep || dfs q
          )
        | Visited -> dfs q
        | InProgress -> true in
  clear_marks g;
  let rec aux l = match l with
    | [] -> false
    | x::q ->  dfs [x] || aux q in
  aux g.g_nodes


let rec print_int_list l = match l with
  | x::q -> (print_int x;print_int_list q)
  | [] -> print_newline ()



let topological g =
  if has_cycle g
    then raise Cycle
  else
    let rep = ref [] in
    let rec visite n =
      if n.n_mark = Visited
        then ()
      else begin n.n_mark <- Visited;
        let rec aux l = match l with
          | [] -> ()
          | x::q -> visite x;aux q in
        aux n.n_link_to;
        rep := n.n_label :: (!rep) end in
    let rec aux l = match l with
      | [] -> ()
      | x::q when x.n_mark = Visited -> aux q
      | x::q -> (visite x; aux q) in
    clear_marks g;
    aux g.g_nodes;
    !rep





 
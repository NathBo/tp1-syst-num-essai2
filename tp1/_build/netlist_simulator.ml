open Netlist_ast

let print_only = ref false
let number_steps = ref (-1)

let env = Hashtbl.create 256

let envreg = Hashtbl.create 256


let ajout_a_env id t = match t with
  | TBit -> Hashtbl.add env id (VBit false);Hashtbl.add envreg id (VBit false)
  | TBitArray n -> Hashtbl.add env id (VBitArray(Array.make n false));Hashtbl.add envreg id (VBitArray(Array.make n false))

let rec input_dans_env l p_vars = match l with
  | [] -> ()
  | id::q -> begin match Env.find id p_vars with
    | TBit -> print_string id;
      print_string " ? ";
      let s = ref (read_line ()) in
      while !s <> "0" && !s<> "1" do
        print_string "Wrong input.\n";
        print_string id;
        print_string " ? ";
        s := read_line () done;
      if !s="0"
      then Hashtbl.replace env id (VBit false)
      else Hashtbl.replace env id (VBit true);
      input_dans_env q p_vars
    | TBitArray n -> print_string id;
      print_string " ? ";
      let s = ref (read_line ()) in
      let rep = Array.make n false in
      let correct = ref (String.length !s = n) in
      for i=0 to n-1 do
        if !correct && !s.[i] = '0'
        then rep.(i) <- false
        else if !correct && !s.[i] = '1'
        then rep.(i) <- true
        else correct := false done;
      while not !correct do
        print_string "Wrong input.\n";
        print_string id;
        print_string " ? ";
        let s = ref (read_line ()) in
        correct := (String.length !s = n);
        for i=0 to n-1 do
          if !correct && !s.[i] = '0'
          then rep.(i) <- false
          else if !correct && !s.[i] = '1'
          then rep.(i) <- true
          else correct := false done;done;
      Hashtbl.replace env id (VBitArray rep);
      input_dans_env q p_vars end



let refersh_envreg id _ =
  Hashtbl.replace envreg id (Hashtbl.find env id)


let xor a b =
  (a || b) && (not (a&&b))


let string_of_bit b =
  if b then "1" else "0"


let compute_value valu = match valu with
  | VBit b -> b
  | _ -> failwith "Pas un bit mais un array"

let compute_array valu = match valu with
  | VBit b -> Array.make 1 b
  | VBitArray a -> a


let compute_arg argu = match argu with
  | Avar id -> Hashtbl.find env id
  | Aconst valu -> valu



let execute exp = match exp with
  | Earg argu -> compute_arg argu
  | Enot argu -> VBit (not(compute_value(compute_arg argu)))
  | Ebinop (op,argu1,argu2) ->  begin match op with
    | Or -> VBit(compute_value (compute_arg argu1) || compute_value (compute_arg argu2))
    | Xor -> VBit(xor (compute_value (compute_arg argu1)) (compute_value (compute_arg argu2)))
    | And -> VBit(compute_value (compute_arg argu1) && compute_value (compute_arg argu2))
    | Nand -> VBit(compute_value (compute_arg argu1) && compute_value (compute_arg argu2)) end
  | Emux (argu1,argu2,argu3) -> if (compute_value (compute_arg argu1))
    then compute_arg(argu3)
    else compute_arg(argu2)
  | Econcat (arg1,arg2) -> VBitArray(Array.append (compute_array(compute_arg arg1)) (compute_array(compute_arg arg2)))
  | Eslice (a,b,argu) -> VBitArray(Array.sub (compute_array (compute_arg argu)) a (b+1))
  | Eselect (i,argu) -> VBit((compute_array (compute_arg argu)).(i))
  | Ereg id -> Hashtbl.find envreg id
  | Erom _ -> failwith "ROM pas implémentée"
  | Eram _ -> failwith "RAM pas implémentée"


let rec calc_eqs l = match l with
    | [] -> ()
    | (id,eq)::q -> Hashtbl.replace env id (execute eq);calc_eqs q


let rec print_outputs l = match l with
    | [] -> ()
    | id::q -> let o = Hashtbl.find_opt env id in
      (match o with
        | None -> failwith ("la valeur "^id^" n'existe pas")
        | Some x -> print_string ("=> "^id^" = "^(string_of_bit (compute_value(x)))^"\n"));
      print_outputs q
    


let simulator program number_steps =
  Env.iter ajout_a_env program.p_vars;
  let i = ref 1 in
  while !i<>number_steps+1 do
    print_string "Step ";
    print_int !i;
    print_string ":\n";

    Env.iter refersh_envreg program.p_vars;
    input_dans_env program.p_inputs program.p_vars;
    calc_eqs program.p_eqs;
    print_outputs program.p_outputs;

    incr i;
  done


let compile filename =
  try
    let p = Netlist.read_file filename in
    begin try
        let p = Scheduler.schedule p in
        simulator p !number_steps
      with
        | Scheduler.Combinational_cycle ->
            Format.eprintf "The netlist has a combinatory cycle.@.";
    end;
  with
    | Netlist.Parse_error s -> Format.eprintf "An error accurred: %s@." s; exit 2

let main () =
  Arg.parse
    ["-n", Arg.Set_int number_steps, "Number of steps to simulate"]
    compile
    ""
;;

main ()

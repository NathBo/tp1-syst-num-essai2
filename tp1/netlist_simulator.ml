open Netlist_ast

let print_only = ref false
let number_steps = ref (-1)

let env = Hashtbl.create 256

let envreg = Hashtbl.create 256

let memory = Hashtbl.create 256

let rams = ref []


let bitarray_to_int v = match v with
  | VBit b -> if b then 1 else 0
  | VBitArray a -> let rep = ref 0 in
    let power2 = ref 1 in
    for i=(Array.length a)-1 downto 0 do
      if(a.(i))then rep := !rep + !power2;
      power2 := !power2*2 done;
    !rep

let rec puissance a n = match n with
    | 0 -> 1
    | _ -> a*puissance a (n-1)



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


let bit_of_char c =
  if c = '0'
    then false
else if c = '1'
  then true
else failwith "Invalid character for a bit (not 0 or 1)"



let execute exp id = match exp with
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
  | Eslice (a,b,argu) -> VBitArray(Array.sub (compute_array (compute_arg argu)) a (b-a+1))
  | Eselect (i,argu) -> VBit((compute_array (compute_arg argu)).(i))
  | Ereg id -> Hashtbl.find envreg id
  | Erom (adrrs,wrds,addr) -> if Hashtbl.mem memory id
    then (Hashtbl.find memory id).(bitarray_to_int (compute_arg addr))
    else begin
      let ic = open_in ("data/"^id^".data") in
      let rep = (Array.make (puissance 2 adrrs) (VBitArray(Array.make wrds false))) in
      for i=0 to puissance 2 adrrs -1 do
        let line = input_line ic in
        let tabl = Array.make wrds false in
        for j=0 to wrds-1 do
          tabl.(j) <- bit_of_char line.[j]
        done;
        rep.(i) <- VBitArray(tabl)
      done;
      Hashtbl.add memory id rep;
      (Hashtbl.find memory id).(bitarray_to_int (compute_arg addr))

    end 
  | Eram (adrrs,wrds,read_addr,write_e,write_addr,data) -> let rep = if Hashtbl.mem memory id
    then (Hashtbl.find memory id).(bitarray_to_int (compute_arg read_addr))
    else VBitArray(Array.make wrds false) in
    rams := (id,exp):: (!rams);
    rep


let rec calc_eqs l = match l with
    | [] -> ()
    | (id,eq)::q -> Hashtbl.replace env id (execute eq id);calc_eqs q


let rec print_outputs l = match l with
    | [] -> ()
    | id::q -> let o = Hashtbl.find_opt env id in
      (match o with
        | None -> failwith ("la valeur "^id^" n'existe pas")
        | Some x -> (match x with 
          | VBit _ -> print_string ("=> "^id^" = "^(string_of_bit (compute_value(x)))^"\n")
          | VBitArray a -> print_string ("=> "^id^" = ");
            for i=0 to Array.length a -1  do
              if a.(i)
              then print_string "1"
              else print_string "0" done;
            print_string "\n");
      print_outputs q)
    

let write_ram eq = match eq with
    | (id,Eram (adrrs,wrds,_,write_e,write_addr,data)) ->
      if compute_value(compute_arg write_e)
        then ((if not (Hashtbl.mem memory id)
          then Hashtbl.add memory id (Array.make (puissance 2 adrrs) (VBitArray(Array.make wrds false))));
          (Hashtbl.find memory id).(bitarray_to_int (compute_arg write_addr)) <- compute_arg data)
    | _ -> failwith "devrait etre un Eram"
    



let simulator program number_steps =
  Env.iter ajout_a_env program.p_vars;
  let i = ref 1 in
  while !i<>number_steps+1 do
    print_string "Step ";
    print_int !i;
    print_string ":\n";

    rams := [];
    Env.iter refersh_envreg program.p_vars;
    input_dans_env program.p_inputs program.p_vars;
    calc_eqs program.p_eqs;
    List.iter write_ram !rams;
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

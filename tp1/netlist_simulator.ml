let print_only = ref false
let number_steps = ref (-1)


let xor a b =
  (a || b) && (not (a&&b))


let compute_value valu = match valu with
  | VBit b -> b
  | _ -> failwith "Pas un bit mais un array"

let compute_array valu = match valu with
  | VBit -> failwith "Pas un array mais un bit"
  | VBitArray a -> a


let compute_arg env argu = match argu with
  | Avar id -> env(id)
  | Aconst valu -> valu



let execute env exp = match exp with
  | Earg argu -> compute_arg argu
  | Enot argu -> VBit (not(compute_value(compute_arg argu)))
  | Ebinop (op,argu1,argu2) -> begin match op with
    | Or -> VBit(compute_value (compute_arg argu1)) || compute_value (compute_arg argu2)
    | Xor -> VBit(xor (compute_value (compute_arg argu1))) (compute_value (compute_arg argu2))
    | And -> VBit(compute_value (compute_arg argu1)) && compute_value (compute_arg argu2)
    | Nand -> VBit(compute_value (compute_arg argu1)) && compute_value (compute_arg argu2) end
  | Emux (argu1,argu2,argu3) -> if (compute_value (compute_arg argu1))
    then compute_arg(argu_3)
    else compute_arg(argu_3)
  | Econcat (arg1,arg2) -> VBitArray(Array.append (compute_array(compute_arg arg1)) (compute_array(compute_arg arg2)))
  | Eslice (a,b,argu) -> VBitArray(Array.sub (compute_array (compute_arg argu)) a b)
  | Eselect (i,argu) -> VBit((compute_array (compute_arg argu)).(i))
  | Ereg -> failwith "reg pas implémenté"
  | Erom -> failwith "ROM pas implémentée"
  | Eram -> failwith "RAM pas implémentée"
    


let simulator program number_steps =


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

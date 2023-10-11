let print_only = ref false
let number_steps = ref (-1)


let compute_value valu = match valu with
  | VBit b -> not b
  | _ -> failwith "grosse flemme"


let compute_arg env argu = match argu with
  | Avar id -> env(id)
  | Aconst valu -> valu



let execute env exp = match exp with
  | Earg argu -> compute_arg argu
  | Enot argu -> VBit (not(compute_value(compute_arg argu)))
  | Ebinop (op,argu1,argu2) -> match op with
    |


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

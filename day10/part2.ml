type computer = { cycles: int; x_val: int; };;
type instruction = { name: string; value: int };;

let file_contents = In_channel.with_open_text "input" In_channel.input_all;;

let instructions = List.map (fun s -> String.split_on_char ' ' s) (String.split_on_char '\n' file_contents);;

let get_add_param instruction = int_of_string (List.hd (List.tl instruction));;

let expand_instruction ins = 
  match List.hd ins with
  | "noop" -> [{name="noop"; value=0}];
  | "addx" -> [{name="noop"; value=0}; {name="addx"; value=get_add_param ins}];
  | _ -> [];
;;

let instructions_per_cycle = List.flatten (List.map (fun ins -> expand_instruction ins) instructions);;

let apply_instruction ins comp = 
  match ins.name with
 | "noop" -> { cycles = comp.cycles + 1; x_val = comp.x_val}
 | "addx" -> { cycles = comp.cycles + 1; x_val = comp.x_val + ins.value}
 | _ -> comp
;;

let sprite_in_range drawing x_pos = 
  x_pos = drawing || x_pos + 1 = drawing || x_pos - 1 = drawing
;;

let print_comp comp = 
  if (comp.cycles mod 40) = 0 then print_newline();
  print_char (match comp.cycles with
  | cycle when sprite_in_range (cycle mod 40) comp.x_val -> '#'
  | _ -> '.');
;;

List.fold_left (fun my_computer instruction -> print_comp my_computer; apply_instruction instruction my_computer) {cycles=0; x_val=1} instructions_per_cycle;;
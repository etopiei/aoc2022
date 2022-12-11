type computer = { cycles: int; x_val: int; };;

let file_contents = In_channel.with_open_text "input" In_channel.input_all;;

let instructions = List.map (fun s -> String.split_on_char ' ' s) (String.split_on_char '\n' file_contents);;

let get_add_param instruction = int_of_string (List.hd (List.tl instruction));;

let in_range current next =
  match next with
  | 1 -> (current mod 40) = 19
  | 2 -> (current mod 40) = 18 || (current mod 40) = 19
  | _ -> false;
;;

let closest_signal cycle = match cycle with
 | a when (a mod 40) = 19 -> cycle + 1
 | a when (a mod 40) = 18 -> cycle + 2
 | _ -> cycle (* This should never happen! *)
;;

let hook_in comp cycle_length_of_next saved_values = match comp.cycles with
 | a when in_range a cycle_length_of_next -> if List.length !saved_values < 6 then saved_values := List.append !saved_values [comp.x_val * (closest_signal comp.cycles)];
 | _ -> ()
;;

let num_cycles_for_instruction ins = match List.hd ins with
 | "noop" -> 1
 | "addx" -> 2
 | _ -> 0
;;

let apply_instruction ins comp saved_values = 
  hook_in comp (num_cycles_for_instruction ins) saved_values;
  match List.hd ins with
 | "noop" -> { cycles = comp.cycles + 1; x_val = comp.x_val}
 | "addx" -> { cycles = comp.cycles + 2; x_val = comp.x_val + (get_add_param ins)}
 | _ -> comp
;;

let saved_values = ref [];;

List.fold_left (fun my_computer instruction -> apply_instruction instruction my_computer saved_values) {cycles=0; x_val=1} instructions;;
List.fold_left (+) 0 !saved_values |> print_int;;
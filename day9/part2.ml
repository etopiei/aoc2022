let file_contents = In_channel.with_open_text "input" In_channel.input_all;;

let parse_instruction str = let parts = String.split_on_char ' ' str in
  ((List.hd parts), int_of_string (List.hd (List.tl parts)))
;;

let instructions = List.map parse_instruction (String.split_on_char '\n' file_contents)

let go_right pos = match pos with
| (x, y) -> (x + 1, y)
;;

let go_left pos = match pos with
| (x, y) -> (x - 1, y)
;;

let go_up pos = match pos with
| (x, y) -> (x, y + 1)
;;

let go_down pos = match pos with
| (x, y) -> (x, y - 1)
;;

let update_along_one_axis head tail =
  let diff = head - tail in
  match diff with
  | 0 -> head
  | 1 -> tail
  | -1 -> tail
  | 2 -> tail + 1
  | -2 -> tail - 1
  | a -> tail
;;

let update_tail_diagonal (head_x, head_y) (tail_x, tail_y) =
  let xdiff = head_x - tail_x in
  let ydiff = head_y - tail_y in
  match xdiff, ydiff with
  | -1, 2 -> (tail_x - 1, tail_y + 1)
  | 1, 2 -> (tail_x + 1, tail_y + 1)
  | -1, -2 -> (tail_x - 1, tail_y - 1)
  | 1, -2 -> (tail_x + 1, tail_y - 1)
  | 2, 1 -> (tail_x + 1, tail_y + 1)
  | 2, -1 -> (tail_x + 1, tail_y - 1)
  | -2, 1 -> (tail_x - 1, tail_y + 1)
  | -2, -1 -> (tail_x - 1, tail_y - 1)
  | 2, 2 -> (tail_x + 1, tail_y + 1)
  | 2, -2 -> (tail_x + 1, tail_y - 1)
  | -2, 2 -> (tail_x - 1, tail_y + 1)
  | -2, -2 -> (tail_x - 1, tail_y - 1)
  | _ -> (tail_x, tail_y)
;;

let update_tail_pair (head_x, head_y) (tail_x, tail_y) = 
  let xdiff = head_x - tail_x in
  let ydiff = head_y - tail_y in
  match abs xdiff, abs ydiff with
  | 0, 0 -> (tail_x, tail_y);
  | 2, 0 -> (update_along_one_axis head_x tail_x), tail_y;
  | 0, 2 -> tail_x, (update_along_one_axis head_y tail_y);
  | 1, 1 -> (tail_x, tail_y); (* Diagonal difference we don't move *)
  | 1, 2 -> update_tail_diagonal (head_x, head_y)  (tail_x, tail_y);
  | 2, 1 ->  update_tail_diagonal (head_x, head_y)  (tail_x, tail_y);
  | 2, 2 -> update_tail_diagonal (head_x, head_y) (tail_x, tail_y);
  | 0, 1 -> (tail_x, tail_y);
  | 1, 0 ->  (tail_x, tail_y);
  | _, _ -> (tail_x, tail_y);
;;

let rec range start finish = match start with
 | a when a = finish -> []
 | _ -> [start]::range (start + 1) finish
;;

let rec apply_instruction_to_head dir head_pos = 
  match dir with
  | "R" -> go_right head_pos;
  | "L" -> go_left head_pos;
  | "U" -> go_up head_pos;
  | _ -> go_down head_pos;
;;

let rec repeat_value n value = match n with
 | 0 -> [];
 | _ -> [value]::(repeat_value (n - 1) value);
;;

let repeated_value_list n value = List.concat (repeat_value n value);;

let make_instruction_into_list (dir, distance) = repeated_value_list distance dir;;

let single_move_instructions = List.concat_map make_instruction_into_list instructions;;

(* First get all the positions of the head in a list
then iterate over all these positions and make a list of 
all tail positions. Repeat this 9 times.
With the last one, make an assoc list to remove duplicates
and print the length of the list!
*)
let rec map_with_past_return_value past_return f remaining_list = match List.length remaining_list with
  | 0 -> [];
  | _ -> f (List.hd remaining_list) past_return::map_with_past_return_value (f (List.hd remaining_list) past_return) f (List.tl remaining_list);
;;

let rec do_n_times_with_return_value n initial_val f = match n with
  | 0 -> initial_val
  | _ -> do_n_times_with_return_value (n - 1) (f initial_val) f
;;

let head_positions = map_with_past_return_value (0, 0) apply_instruction_to_head single_move_instructions;;

let tail_9_positions = do_n_times_with_return_value 9 head_positions (map_with_past_return_value (0, 0) update_tail_pair);;

let visited = ref [((0, 0), true)];;

List.iter (fun pos -> visited := List.remove_assoc pos !visited; visited := List.cons (pos, true) !visited;) tail_9_positions;;
List.length !visited |> print_int;;

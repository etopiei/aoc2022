let file_contents = In_channel.with_open_text "input" In_channel.input_all;;

let head_pos = ref (0, 0);;
let tail_pos = ref (0, 0);;
let tail_visited = ref [((0, 0), true)];;

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
  | _ -> tail - 1
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
  | _ -> (tail_x, tail_y)
;;

let update_tail (head_x, head_y) (tail_x, tail_y) = 
  let xdiff = head_x - tail_x in
  let ydiff = head_y - tail_y in
  match abs (xdiff), abs (ydiff) with
  | _, 0 -> (update_along_one_axis head_x tail_x), tail_y;
  | 0, _ -> tail_x, (update_along_one_axis head_y tail_y);
  | 1, 1 -> (tail_x, tail_y); (* Diagonal difference we don't move *)
  | 1, 2 -> update_tail_diagonal (head_x, head_y)  (tail_x, tail_y);
  | 2, 1 ->  update_tail_diagonal (head_x, head_y)  (tail_x, tail_y);
  | _ -> (tail_x, tail_y);
;;

let update_visited tail_visited (tail_x, tail_y) = 
  tail_visited := List.remove_assoc (tail_x, tail_y) !tail_visited;
  tail_visited := List.cons ((tail_x, tail_y), true) !tail_visited;
;;

let apply_instruction (dir, distance) head_pos tail_pos tail_visited = 
match dir with
| "R" -> for i = 1 to distance do head_pos := (go_right !head_pos); tail_pos := update_tail !head_pos !tail_pos; update_visited tail_visited !tail_pos; done;
| "L" -> for i = 1 to distance do head_pos := (go_left !head_pos); tail_pos := update_tail !head_pos !tail_pos; update_visited tail_visited !tail_pos; done;
| "U" -> for i = 1 to distance do head_pos := (go_up !head_pos); tail_pos := update_tail !head_pos !tail_pos; update_visited tail_visited !tail_pos; done;
| _ -> for i = 1 to distance do head_pos := (go_down !head_pos); tail_pos := update_tail !head_pos !tail_pos; update_visited tail_visited !tail_pos; done;
;;

let print_head () = match !head_pos with
| (x, y) -> print_int x; print_int y;
;;

(* Iterate over applying the instructions to the head *)
List.iter (fun (dir, distance) -> apply_instruction (dir, distance) head_pos tail_pos tail_visited;) instructions;;

List.length !tail_visited |> print_int
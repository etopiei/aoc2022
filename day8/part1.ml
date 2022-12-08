let file_contents = In_channel.with_open_text "input" In_channel.input_all;;

let found_at = ref []

let lines = String.split_on_char '\n' file_contents;;

let update_min item min = 
  if item > !min then
    min := item;;

let get_num_increasing_items l = 
  let min = ref (-1) in
  List.mapi (fun i item -> let old_min = !min in update_min item min; if item > old_min then i else (-1)) l
  |> List.filter (fun a -> a != (-1))
;;

let make_ints lst = List.of_seq (Seq.map (fun chr -> (int_of_char chr) - 48) lst)
;;

let add_to_found (x, y) = 
  found_at := List.remove_assoc (x, y) !found_at;
  found_at := List.cons ((x, y), true) !found_at
;;

let go_left line line_index = make_ints (String.to_seq line)
|> get_num_increasing_items
|> List.iter (fun i -> add_to_found (i, line_index))
;;

let go_right line line_index = make_ints (List.to_seq (List.rev (List.of_seq (String.to_seq line))))
|> get_num_increasing_items
|> List.iter (fun i  -> add_to_found (((String.length line) - i - 1), line_index))
;;


(* Iterate left to right *)
List.iteri (fun ind line -> go_left line ind; go_right line ind;) lines;;

let go_down lst col_ind = make_ints (List.to_seq lst)
|> get_num_increasing_items
|> List.iter (fun i  -> add_to_found (col_ind, i))
;;

let go_up lst col_ind = make_ints (List.to_seq lst)
|> get_num_increasing_items
|> List.iter (fun i -> add_to_found (col_ind, ((List.length lst) - i - 1)))
;;

(* Now we need to iterate up/down which is a bit tricker! 
  Thankfully we can assume all lines have the same length
  and to get each 'column' we can make a list of chars from each index
*)
for i = 0 to String.length (List.hd lines) - 1 do
  let col_char = List.map (fun line -> String.get line i) lines in
  go_down col_char i;
  go_up (List.rev col_char) i;
done;;

print_int (List.length !found_at);;
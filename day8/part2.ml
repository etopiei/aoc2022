let file_contents = In_channel.with_open_text "input" In_channel.input_all;;

let found_at = ref [];;

let lines = String.split_on_char '\n' file_contents;;

let get_list_from_point l ind = List.filteri (fun i _ -> i > ind) l;;

let get_distance item remaining_list =
  let available_trees = (List.to_seq remaining_list) in
  let until_blocked = Seq.take_while (fun i -> i < item) available_trees in
  match Seq.length available_trees = Seq.length until_blocked with
  | true -> Seq.length until_blocked
  | false -> Seq.length until_blocked + 1
;;

let remove_end l = 
  List.rev (List.tl (List.rev l))
;;

let get_viewing_distances l = 
  (* Re-write this to get the viewing distance of all items in the direction of the list
    the main trick to watch out for here is that if an item is on the edge (last item), it gets 0 in that direction!
  *)
  let lst = List.mapi (fun i item -> get_distance item (get_list_from_point l i) ) l
  |> remove_end in
  List.append lst [0]
;;

let make_ints lst = List.of_seq (Seq.map (fun chr -> (int_of_char chr) - 48) lst)
;;

let update_found (x, y) view_distance = 
  let current = match List.mem_assoc (x, y) !found_at with
  | false -> 1
  | true -> List.assoc (x, y) !found_at in

  found_at := List.remove_assoc (x, y) !found_at;
  found_at := List.cons ((x, y), (current * view_distance)) !found_at
;;

let go_left line line_index = make_ints (String.to_seq line)
|> get_viewing_distances
|> List.iteri (fun i view_distance -> update_found (i, line_index) view_distance)
;;

let go_right line line_index = make_ints (List.to_seq (List.rev (List.of_seq (String.to_seq line))))
|> get_viewing_distances
|> List.iteri (fun i view_distance  -> update_found (((String.length line) - i - 1), line_index) view_distance)
;;


(* Iterate left to right *)
List.iteri (fun ind line -> go_left line ind; go_right line ind;) lines;;

let go_down lst col_ind = make_ints (List.to_seq lst)
|> get_viewing_distances
|> List.iteri (fun i view_distance  -> update_found (col_ind, i) view_distance)
;;

let go_up lst col_ind = make_ints (List.to_seq lst)
|> get_viewing_distances
|> List.iteri (fun i view_distance -> update_found (col_ind, ((List.length lst) - i - 1)) view_distance)
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

List.map (fun (k, v) -> v) !found_at
|> List.fold_left max 0
|> print_int

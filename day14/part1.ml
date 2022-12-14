let file_contents = In_channel.with_open_text "input"  In_channel.input_all;;

let rock_descriptions = String.split_on_char '\n' file_contents;;

type sand = {x: int; y: int};;

let rocks = Hashtbl.create 30;;

let rec get_coord_one_axis start finish = 
  match start - finish with
  | 0 -> [finish]
  | a when a > 0  -> [start] @ (get_coord_one_axis (start - 1) finish)
  | _ -> [start] @ (get_coord_one_axis (start + 1) finish)
;;

let place_rocks rocks (start_x, start_y) (end_x, end_y) = 
  List.iter (fun x -> List.iter (fun y -> (Hashtbl.add rocks (x, y) true)) (get_coord_one_axis start_y end_y)) (get_coord_one_axis start_x end_x) 
;;

let first_two_as_tuple lst = (List.hd lst,  (List.hd (List.tl lst)));;

let parse_line rocks line = 
  let parts = String.split_on_char ' ' line in
  for i = 1 to List.length parts / 2 do
    let start_part_index = ((i - 1) * 2) in
    let end_part_index = (i * 2) in

    let start_pos = List.map int_of_string (String.split_on_char ',' (List.nth parts start_part_index))
    |> first_two_as_tuple in

    let end_pos = List.map int_of_string (String.split_on_char ',' (List.nth parts end_part_index))
    |> first_two_as_tuple in

    place_rocks rocks start_pos end_pos;
  done
;;


let simulate_sand rocks = 
  List.iter (fun s -> parse_line rocks s) rock_descriptions; (* Set rocks down *)

  let floor = Seq.map (fun (x, y) -> y) (Hashtbl.to_seq_keys rocks) |> Seq.fold_left max 0 in
  let sand_ref = ref {x=500; y=0} in
  let rested_sand = ref 0 in

  (* If it goes below floor, quit *)
  while !sand_ref.y < floor do
    (* Move sand down if possible *)
    match Hashtbl.mem rocks (!sand_ref.x, !sand_ref.y + 1) with
    | false -> sand_ref := {x= !sand_ref.x; y=(!sand_ref.y + 1)};
    | true -> match Hashtbl.mem rocks (!sand_ref.x - 1, !sand_ref.y + 1) with
      | false -> sand_ref := {x= !sand_ref.x - 1; y= !sand_ref.y + 1};
      | true -> match Hashtbl.mem rocks (!sand_ref.x + 1, !sand_ref.y + 1) with
        | false -> sand_ref := {x= !sand_ref.x + 1; y= !sand_ref.y + 1};
        | true -> (Hashtbl.add rocks (!sand_ref.x, !sand_ref.y) true; sand_ref := {x=500; y=0}; rested_sand := !rested_sand + 1);
  done;

  print_int !rested_sand;
;;

simulate_sand rocks;;
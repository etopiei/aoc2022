(* Plan:
1. Parse input
2. Iterate over all sensors and write to a hash map where there can be no beacons (on y=20000) based on intercept and maths
3. Print length of hash-map
5. Return result
*)

let target = 2000000;;

let file_contents = In_channel.with_open_text "input" In_channel.input_all;;
let lines = String.split_on_char '\n' file_contents;;

type position = {x:int;y:int};;

let get_x_y_from_part part = 
  let x = int_of_string (List.hd (String.split_on_char ',' (Str.string_after part ((Str.search_forward (Str.regexp "x=") part 0) + 2)))) in
  let y = int_of_string (Str.string_after part ((Str.search_forward (Str.regexp "y=") part 0) + 2)) in
  {x; y}
;;

let parse_line line = 
  let items = String.split_on_char ':' line in
  (get_x_y_from_part (List.hd items), get_x_y_from_part (List.hd (List.tl items)))
;;

let sensor_beacon_position_pairs = List.map parse_line lines;;

let non_beacon_target_y = Hashtbl.create 30;;

let man_distance pos1 pos2 = 
  abs (pos1.x - pos2.x) + abs (pos1.y - pos2.y)
;;

let fill_ht_from_x x remaining_distance =
  for offset = 0 to remaining_distance do
    Hashtbl.replace non_beacon_target_y (x + offset) true;
    Hashtbl.replace non_beacon_target_y (x - offset) true;
  done
;;

List.iter (fun (sensor_pos, beacon_pos) -> 
  let distance_to_beacon = man_distance sensor_pos beacon_pos in
  let distance_to_target_in_y = abs (target - sensor_pos.y) in
  let remaining_distance_after_y = distance_to_beacon - distance_to_target_in_y in
  if remaining_distance_after_y > 0 then fill_ht_from_x sensor_pos.x remaining_distance_after_y;
) sensor_beacon_position_pairs;;

List.iter (fun (_, beacon_pos) -> if beacon_pos.y = target then Hashtbl.remove non_beacon_target_y beacon_pos.x) sensor_beacon_position_pairs;;

Hashtbl.length non_beacon_target_y |> print_int;;


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

let man_distance pos1 pos2 = 
  abs (pos1.x - pos2.x) + abs (pos1.y - pos2.y)
;;

let tuning_frequency x y = (4000000 * x) + y;;

let xy_diamond_from_point sensor_pos distance = 
  let points = ref [] in
  let x = sensor_pos.x in
  let y = sensor_pos.y in
  for offset = 0 to distance do
    points := List.cons (x + distance - offset, y + offset) !points;
    points := List.cons (x - distance + offset, y + offset) !points;
    points := List.cons (x + distance - offset, y - offset) !points;
    points := List.cons (x - distance + offset, y - offset) !points;
  done;
  !points
;;

let no_overlaps_found (x, y) = List.for_all (fun v -> v) (List.map (fun (sensor_pos, beacon_pos) -> 
      let distance_to_beacon = man_distance sensor_pos beacon_pos in
      let distance_to_target = man_distance sensor_pos {x;y} in
      distance_to_beacon < distance_to_target
    ) sensor_beacon_position_pairs)
;;

let in_range (x, y) = 
  x > 0 && x < 4000000 && y > 0 && y < 4000000
;;

List.iter (fun (sensor_pos, beacon_pos) -> 
  let distance_to_beacon = man_distance sensor_pos beacon_pos in
  let diamond_points = xy_diamond_from_point sensor_pos (distance_to_beacon + 1)  in
  List.iter (fun (x, y) -> if no_overlaps_found (x, y) && in_range (x, y) then
      (print_int (tuning_frequency x y); exit 0;)
  ) diamond_points;
) (sensor_beacon_position_pairs);;


let file_contents = In_channel.with_open_text "input" In_channel.input_all;;
let lines = String.split_on_char '\n' file_contents

let check_ranges (a::b, c::d) =
  let d = List.hd d in
  let b = List.hd b in
  ((a >= c) && (a <= d) && (b >= c) && (b <= d))
  ||
  ((c >= a) && (c <= b) && (d >= a) && (d <= b));;

let parse_ranges range_line = 
  let a = List.hd (String.split_on_char ',' range_line) in
  let b = List.hd (List.rev (String.split_on_char ',' range_line)) in
  let rangeA = List.map int_of_string (String.split_on_char '-' a) in
  let rangeB = List.map int_of_string (String.split_on_char '-' b) in
  (rangeA, rangeB);;
let overlapping_lines = List.filter check_ranges (List.map parse_ranges lines);;

print_int (List.length overlapping_lines)
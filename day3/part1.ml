let file_contents = In_channel.with_open_text "input" In_channel.input_all;;
let lines = String.split_on_char '\n' file_contents;;

let convert_char_to_priority chr = if Char.code chr < 97 then
  (Char.code chr) - 38
else
  (Char.code chr) - 96

let split_lists lists = 
  let first_half = List.filteri (fun i item -> i < ((List.length lists) / 2)) lists in
  let second_half = List.filteri (fun i item -> i >= ((List.length lists) / 2)) lists in
  (first_half, second_half);;

let find_duplicate_char chrs = 
  let (list_a, list_b) = split_lists chrs in
  List.hd (List.filter (fun item -> List.exists ((=) item) list_b) list_a)
;;

let make_char_array line = List.of_seq (String.to_seq line);;

let compute_priority line = make_char_array line
|> find_duplicate_char
|> convert_char_to_priority;;
let priorities = List.map (fun line -> compute_priority line) lines;;

print_int (List.fold_left (+) 0 priorities)

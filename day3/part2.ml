let file_contents = In_channel.with_open_text "input" In_channel.input_all;;
let lines = String.split_on_char '\n' file_contents;;

let convert_char_to_priority chr = if Char.code chr < 97 then
  (Char.code chr) - 38
else
  (Char.code chr) - 96

let find_common_char list_a list_b list_c = 
  List.hd (List.filter (fun item -> (List.exists ((=) item) list_b) && List.exists ((=) item) list_c) list_a)
;;

let make_char_array line = List.of_seq (String.to_seq line);;

let chunk_by_3 lst = 
  let list_1 = List.filteri (fun i _ -> i mod 3 = 0) lst in
  let list_2 = List.filteri (fun i _ -> i mod 3 = 1) lst in
  let list_3 = List.filteri (fun i _ -> i mod 3 = 2) lst in
  (list_1, list_2, list_3)
;;


let chrs_lists = List.map make_char_array lines;;

let rucksacks = chunk_by_3 chrs_lists;;

let priorities = 
  let (list_a, list_b, list_c) = rucksacks in
  List.mapi (fun i _ -> find_common_char (List.nth list_a i) (List.nth list_b i) (List.nth list_c i)) list_a 
|> List.map convert_char_to_priority;;

print_int (List.fold_left (+) 0 priorities)

let file = "input"
let elf_lines = In_channel.with_open_text file In_channel.input_all 
  |> Str.split (Str.regexp "\n\n");;

let convert_to_ints lines = List.map int_of_string lines;;
let rec sum list = match list with
[] -> 0
| h :: t -> h + (sum t);;

let max list = List.fold_left max 0 list;;

let count_calories cal_list = sum (convert_to_ints cal_list);;

let calorie_list_lists = List.map (Str.split (Str.regexp "\n")) elf_lines;;
let calorie_count_lists = List.map count_calories calorie_list_lists;;

print_int (max calorie_count_lists);;





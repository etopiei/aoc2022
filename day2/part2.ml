
let file = "input";;
let file_contents = In_channel.with_open_text file In_channel.input_all;;
let games = String.split_on_char '\n' file_contents;;
let game_tuples = List.map (String.split_on_char ' ') games;;


let opponent_choices = List.map List.hd game_tuples;;
let my_choices = List.map (fun a -> List.nth a 1) game_tuples;;


let score_game opponent_choice player_choice = match (opponent_choice, player_choice) with
  | ('B', 'X') -> 1
  | ('C', 'Y') -> 6
  | ('A', 'Z') -> 8 
  | ('A', 'X') -> 3
  | ('B', 'Y') -> 5
  | ('C', 'Z') -> 7
  | ('C', 'X') -> 2
  | ('A', 'Y') -> 4
  | ('B', 'Z') -> 9
  | _ -> 0
;;

let games = Seq.zip (List.to_seq opponent_choices) (List.to_seq my_choices);;

let get_char char_string = List.hd (List.of_seq (String.to_seq char_string));;

let scores = Seq.map (fun (a, b) -> score_game (get_char a) (get_char b)) games;;

let total_score = Seq.fold_left (+) 0 scores;;

print_int total_score




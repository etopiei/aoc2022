In_channel.with_open_text "input" In_channel.input_all
 |> String.split_on_char '\n'
 |> List.map (String.split_on_char ' ')
 |> List.map (fun (a::b) -> (List.hd (List.of_seq (String.to_seq a)), List.hd (List.of_seq (String.to_seq b))))
 |> List.map (fun (a, b) -> match (a, b) with
  | ('B', 'X') -> 1
  | ('C', 'Y') -> 2
  | ('A', 'Z') -> 3
  | ('A', 'X') -> 4 
  | ('B', 'Y') -> 5
  | ('C', 'Z') -> 6
  | ('C', 'X') -> 7
  | ('A', 'Y') -> 8 
  | ('B', 'Z') -> 9
  | _ -> 0
  )
  |> List.fold_left (+) 0
  |> print_int

In_channel.with_open_text "input" In_channel.input_all 
  |> Str.split (Str.regexp "\n\n")
  |> List.map (Str.split (Str.regexp "\n"))
  |> List.map (List.map int_of_string) 
  |> List.map (List.fold_left (+) 0)
  |> List.sort (Fun.flip compare)
  |> List.to_seq
  |> Seq.take 3
  |> List.of_seq
  |> List.fold_left (+) 0
  |> print_int

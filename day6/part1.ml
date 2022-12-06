let file_contents = In_channel.with_open_text "input" In_channel.input_all;;

for i = 0 to String.length file_contents - 4 do
  let a = String.get file_contents (i) in
  let b = String.get file_contents (i + 1) in
  let c = String.get file_contents (i + 2) in
  let d = String.get file_contents (i + 3) in
  if a != b && a != c && a != d && b != c && b != d && c != d  then (print_int (i + 4); exit 0)
done

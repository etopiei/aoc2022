let file_contents = In_channel.with_open_text "input" In_channel.input_all;;

let list_without_i list i = List.filteri (fun ind item -> ind != i) list;;

for i = 0 to String.length file_contents - 14 do
  let a = String.get file_contents (i) in
  let b = String.get file_contents (i + 1) in
  let c = String.get file_contents (i + 2) in
  let d = String.get file_contents (i + 3) in
  let e = String.get file_contents (i + 4) in
  let f = String.get file_contents (i + 5) in
  let g = String.get file_contents (i + 6) in
  let h = String.get file_contents (i + 7) in
  let i2 = String.get file_contents (i + 8) in
  let j = String.get file_contents (i + 9) in
  let k = String.get file_contents (i + 10) in
  let l = String.get file_contents (i + 11) in
  let m = String.get file_contents (i + 12) in
  let n = String.get file_contents (i + 13) in
  let list_of_chars = a :: b :: c :: d :: e :: f :: g :: h :: i2 :: j :: k :: l :: m :: [n] in
  let is_duplicate = List.mapi (fun ind chr -> List.exists (fun item -> item = chr) (list_without_i list_of_chars ind)) list_of_chars in
  if List.for_all (fun x -> not x) is_duplicate
  then (print_int (i + 14); exit 0)
done

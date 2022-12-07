let file_contents = In_channel.with_open_text "input" In_channel.input_all;;

let lines = String.split_on_char '\n' file_contents;;

(*
Start iterating over lines.

Command Rules:
If we encounter a '$ cd <x>' add '/<x>' to the in_dir variable
If we encounter a '$ cd ..' remove the last slash onwards from in_dir
If $ls do nothing

Dir Rules:
We can safely ignore anything that starts with 'dir' because we can only sum up by going into the dir

File rules:
If we encounter '<size> <filename>' we add the 'size' to an assoc list, using key: 'in_dir'
*)
let in_dir = ref "";;
let dir_size_list = ref [("/", 0)];;

let strip_trailing_dir path = String.cat "/" (String.concat "/" (List.rev (List.tl (List.rev (List.tl (String.split_on_char '/' path))))));;

let handle_cd to_dir in_dir =
match to_dir with
 | ".." -> in_dir := strip_trailing_dir !in_dir;
 | "/" -> in_dir := "/";
 | _ -> in_dir := String.concat "/" [!in_dir; to_dir];
;;

let handle_command line in_dir = match List.nth (String.split_on_char ' ' line) 1 with
  | "cd" -> handle_cd (List.nth (String.split_on_char ' ' line) 2) in_dir
  | "ls" -> ()
  | _ -> ()
;;


let add_size_to_dir size in_dir = 
  let current_size = match List.mem_assoc in_dir !dir_size_list with
   | true -> List.assoc in_dir !dir_size_list
   | _ -> 0 in

  dir_size_list := List.remove_assoc in_dir (!dir_size_list);
  dir_size_list := List.cons (in_dir, (current_size + size)) !dir_size_list
;;

let rec get_all_parents in_dir acc = match in_dir with
 | "/" -> acc
 | _ -> get_all_parents (strip_trailing_dir in_dir) (List.append [in_dir] acc)
;;

let handle_file size in_dir =
  List.iter (add_size_to_dir size) (get_all_parents in_dir ["/"]);;

let handle_line line = match List.hd (String.split_on_char ' ' line) with
 | "$" -> handle_command line in_dir
 | "dir" -> ()
 | _ -> handle_file (int_of_string (List.hd (String.split_on_char ' ' line))) !in_dir
;;

List.iter (fun line -> handle_line line) lines;;

let used_space = List.fold_left max 0 (List.map (fun (a, b) -> b) !dir_size_list);;
let space_free = 70000000 - used_space;;
let min_deletion = 30000000 - space_free;;

let deletion_size = List.filter (fun size -> size >= min_deletion) (List.map (fun (a, b) -> b) !dir_size_list)
|> List.fold_left min Int.max_int
|> print_int 

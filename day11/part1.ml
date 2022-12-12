let file_contents = In_channel.with_open_text "input" In_channel.input_all;;
let monkey_infos = List.map (String.split_on_char '\n') (Str.split (Str.regexp "\n\n") file_contents);;

(*
1. Parse monkey infos into data structures
2. Write a function to do iterate through 1 monkeys items (and keep track of inspections)
3. Write a function to do a 'round'
4. Repeat 20 round 20 times
5. Map monkey -> num inspections
6. Sort and take top 2
7. Return the multiple of these two values
*)

type monkey = {mutable items: int list; mutable num_inspections: int; check_operator: string; check_value: int; result_divisible_by: int; true_index: int; false_index: int};;

let last_in_lst lst = List.hd (List.rev lst);;

let make_monkey_from_lines lines =
  let items = List.map (fun s -> int_of_string (String.trim s)) (String.split_on_char ',' (List.hd (List.tl (String.split_on_char ':' (List.nth lines 1))))) in
  let check_value_s = last_in_lst (String.split_on_char ' ' (List.nth lines 2)) in
  let check_operator = match check_value_s with
    | "old" -> "**"
    | _ -> match String.contains (List.nth lines 2) '*' with
      | true -> "*"
      | _ -> "+"
    in
  let check_value = match check_value_s with
    | "old" -> 2
    | _ -> int_of_string check_value_s
  in
  let result_divisible_by = int_of_string (last_in_lst (String.split_on_char ' ' (List.nth lines 3))) in
  let true_index = int_of_string (last_in_lst (String.split_on_char ' ' (List.nth lines 4))) in
  let false_index = int_of_string (last_in_lst (String.split_on_char ' ' (List.nth lines 5))) in
  {items;num_inspections=0;check_operator;check_value;result_divisible_by;true_index;false_index}
;;

let monkeys = List.map (fun lines -> make_monkey_from_lines lines) monkey_infos;;

let rec power x n = match n with
 | 1 -> x
 | _ -> x * power x (n-1)
;;

let apply_rules monkey monkey_lst item = 
  monkey.num_inspections <- monkey.num_inspections + 1;
  let test_value = match monkey.check_operator with 
  | "**" -> (power item monkey.check_value) / 3;
  | "*" -> (item * monkey.check_value) / 3;
  | _ -> (item + monkey.check_value) / 3 in
  let index_to_send_to = match (test_value mod monkey.result_divisible_by) = 0 with
  | true -> monkey.true_index
  | _ -> monkey.false_index in
  (List.nth monkey_lst index_to_send_to).items <- List.append (List.nth monkey_lst index_to_send_to).items [test_value];
;;

let run_round monkey_lst = 
  List.iter (fun monkey -> 
    List.iter (fun item -> apply_rules monkey monkey_lst item) monkey.items;
    monkey.items <- [];
  ) monkey_lst;
  monkey_lst
;;

let rec do_with_value_n_times value n f = match n with
 | 0 -> value
 | _ -> do_with_value_n_times (f value) (n-1) f
;;

let final_monkeys = do_with_value_n_times monkeys 20 run_round;;
let num_inspections = List.map (fun monkey -> monkey.num_inspections) final_monkeys;;

let sorted = List.sort (Fun.flip compare) num_inspections;;
let top_2 = List.filteri (fun i _ -> i < 3) sorted;;

(List.hd top_2) * (List.hd (List.tl top_2)) |> print_int;;

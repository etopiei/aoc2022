let file_contents = In_channel.with_open_text "input" In_channel.input_all;;
let input_parts = Str.split (Str.regexp "\n\n") file_contents;;

let stacks_input_lines = String.split_on_char '\n' (List.hd input_parts);;
let instructions = String.split_on_char '\n' (List.hd (List.tl input_parts));;


let parse_line_for_stack_x stacks in_line x = 
  match List.nth (List.of_seq (String.to_seq in_line)) (x * 4 + 1) with
   | ' ' -> ()
   | chr -> Stack.push chr (List.nth stacks x)
;;

let parse_line_add_to_stacks stacks in_line = 
  List.iteri (fun i _ -> parse_line_for_stack_x stacks in_line i) stacks;; 


(* Initialize stacks *)
let stacks = List.init 9 (fun _ -> Stack.of_seq (List.to_seq [' ']));;
List.iter (fun stack_input_line -> parse_line_add_to_stacks stacks stack_input_line) (List.rev stacks_input_lines);;

(* Apply instructions to stacks *)
let apply_instruction (times, from_stack, to_stack) = 
  let aux_stack = Stack.of_seq (List.to_seq [' ']) in

  for i = 1 to times do
    Stack.push (Stack.pop (List.nth stacks from_stack)) aux_stack
  done;

  for i = 1 to times do
    Stack.push (Stack.pop aux_stack) (List.nth stacks to_stack)
  done
;;


let parse_instruction instruction = 
  let parts = String.split_on_char ' ' instruction in
  let times = int_of_string (List.nth parts 1) in
  let from_stack = int_of_string (List.nth parts 3) - 1 in
  let to_stack = int_of_string (List.nth parts 5) - 1 in 
  times, from_stack, to_stack
;;

List.iter (fun instruction -> apply_instruction (parse_instruction instruction)) instructions;;

(* Return stack tops *)
List.map Stack.pop stacks |> List.iter print_char

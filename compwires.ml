open Batteries

let rec read_yn prompt =
  print_endline prompt;
  let line = input_line stdin in
  match line with
  | "y" | "Y" -> true
  | "n" | "N" -> false
  | _         -> read_yn prompt

let parallel = lazy (read_yn "Is there a parallel port?")

let batteries_gt_2 = lazy (read_yn "Are there 2 or more batteries?")

let even_last_digit =
  lazy (read_yn "Is the last digit of the serial number even?")

module IntMap = Map.Make (Int)

let cond_to_id cond =
  (if String.contains cond 'r' then 8 else 0)
  + (if String.contains cond 'b' then 4 else 0)
  + (if String.contains cond 's' then 2 else 0)
  + if String.contains cond 'l' then 1 else 0

let line_to_data line =
  let conds = String.sub line 0 4
  and[@warning "-8"] result =
    match line.[5] with
    | 'c' -> lazy true
    | 's' -> lazy false
    | 'p' -> parallel
    | 'b' -> batteries_gt_2
    | 'e' -> even_last_digit
  in
  (cond_to_id conds, result)

let id_to_action =
  File.lines_of "compwires.txt"
  |> Enum.fold
       (fun acc v ->
         let id, action = line_to_data v in
         IntMap.add id action acc)
       IntMap.empty

let () =
  try
    while true do
      print_endline
        "Enter key characteristics (r = has red, b = has blue, s = has star, l \
         = has led)";
      if Lazy.force (IntMap.find (cond_to_id (input_line stdin)) id_to_action)
      then print_endline "Cut"
      else print_endline "Skip"
    done
  with End_of_file -> ()

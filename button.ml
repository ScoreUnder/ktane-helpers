open Printf

type color_t = Blue | Red | White | Yellow | Other
[@@deriving show { with_path = false }]

type text_t = Abort | Detonate | Hold | Other
[@@deriving show { with_path = false }]

let rec read_yn prompt =
  print_endline prompt;
  let line = input_line stdin in
  match String.lowercase_ascii line with
  | "y" -> true
  | "n" -> false
  | _   -> read_yn prompt

let to_color s =
  match Char.lowercase_ascii s.[0] with
  | 'b' -> Blue
  | 'r' -> Red
  | 'w' -> White
  | 'y' -> Yellow
  | _   -> Other

let to_text_enum s =
  match Char.lowercase_ascii s.[0] with
  | 'a' -> Abort
  | 'd' -> Detonate
  | 'h' -> Hold
  | _   -> Other

let batteries_gte : int -> bool =
  let min_batteries = ref 0 in
  let max_batteries = ref 9 in
  fun n ->
    if n > !min_batteries && n <= !max_batteries then
      if read_yn ("Are there at least " ^ string_of_int n ^ " batteries?") then
        min_batteries := n
      else max_batteries := n - 1;
    n <= !min_batteries && n <= !max_batteries

module StringMap = Map.Make (String)

let lit_indicator : string -> bool =
  let indicators = ref StringMap.empty in
  fun s ->
    match StringMap.find_opt s !indicators with
    | Some x -> x
    | None   ->
        let lit = read_yn ("Is there a lit indicator labelled " ^ s ^ "?") in
        indicators := StringMap.add s lit !indicators;
        lit

let c_bold = "\027[1m"

let c_reset = "\027[0m"

let hold_button () =
  print_endline "What colour is the strip? (blue, yellow, other)";
  let digit =
    match to_color (input_line stdin) with
    | Blue   -> "4"
    | Yellow -> "5"
    | _      -> "1"
  in
  print_endline
    (c_bold ^ "When timer has " ^ digit ^ " in any position, release the button"
   ^ c_reset)

let press_button () =
  print_endline (c_bold ^ "Press and release the button" ^ c_reset)

let main () =
  print_endline "Type button colour (blue, red, other)";
  let color = to_color (input_line stdin) in
  print_endline "Type button text (Abort, Detonate, Hold, other)";
  let text = to_text_enum (input_line stdin) in
  printf "\n%s %s\n\n%!" (show_color_t color) (show_text_t text);
  match (color, text) with
  | Blue, Abort -> hold_button ()
  | _, Detonate when batteries_gte 2 -> press_button ()
  | White, _ when lit_indicator "CAR" -> hold_button ()
  | _, _ when batteries_gte 3 && lit_indicator "FRK" -> press_button ()
  | Yellow, _ -> hold_button ()
  | Red, Hold -> press_button ()
  | _, _ -> hold_button ()

;;
while true do
  main ()
done

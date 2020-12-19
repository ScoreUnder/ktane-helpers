open Batteries
open Printf

let def_red = Vect.of_list [ "c"; "b"; "a"; "ac"; "b"; "ac"; "abc"; "ab"; "b" ]

let def_blue = Vect.of_list [ "b"; "ac"; "b"; "a"; "b"; "bc"; "c"; "ac"; "a" ]

let def_black =
  Vect.of_list [ "abc"; "ac"; "b"; "ac"; "b"; "bc"; "ab"; "c"; "c" ]

let qwerty_transform c =
  match c with
  | 'q' -> 'a'
  | 'w' -> 'b'
  | 'e' -> 'c'
  | 'p' -> 'a'
  | '[' -> 'b'
  | ']' -> 'c'
  | x   -> x

let colour_transform c =
  match c with
  | 'b' -> '2'
  | 'r' -> '1'
  | 'k' -> '3'
  | 'q' -> '1'
  | 'w' -> '2'
  | 'e' -> '3'
  | x   -> x

let c_red = "\027[31;1m"

let c_blue = "\027[34;1m"

let c_black = "\027[47;30m"

let c_redbg = "\027[41;1m"

let c_reset = "\027[0m"

let cut_if b = print_endline (if b then c_redbg ^ "Cut" ^ c_reset else "Skip")

let () =
  let rec do_wires rpos bpos kpos last =
    print_endline
      "Enter the wire colour (1 = red, 2 = blue, 3 = black) and connection (A, \
       B, C)";
    let line = String.lowercase (input_line stdin) in
    let check lst pos =
      cut_if (String.contains (Vect.get lst pos) (qwerty_transform line.[1]))
    and notify c =
      printf "%s %c\n" c (Char.uppercase (qwerty_transform line.[1]))
    in
    if String.length line = 2 then
      match colour_transform line.[0] with
      | '1' ->
          notify (c_red ^ "Red" ^ c_reset);
          check def_red rpos;
          do_wires (1 + rpos) bpos kpos 1
      | '2' ->
          notify (c_blue ^ "Blue" ^ c_reset);
          check def_blue bpos;
          do_wires rpos (1 + bpos) kpos 2
      | '3' ->
          notify (c_black ^ "Black" ^ c_reset);
          check def_black kpos;
          do_wires rpos bpos (1 + kpos) 3
      | '-' -> (
          print_endline "UNDO";
          match last with
          | 1 -> do_wires (rpos - 1) bpos kpos last
          | 2 -> do_wires rpos (bpos - 1) kpos last
          | 3 -> do_wires rpos bpos (kpos - 1) last
          | _ -> do_wires rpos bpos kpos last)
      | _   -> do_wires rpos bpos kpos last
    else do_wires rpos bpos kpos last
  in
  do_wires 0 0 0 0

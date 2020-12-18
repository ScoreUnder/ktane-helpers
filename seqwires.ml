open Batteries

let def_red = [ "c"; "b"; "a"; "ac"; "b"; "ac"; "abc"; "ab"; "b" ]

let def_blue = [ "b"; "ac"; "b"; "a"; "b"; "bc"; "c"; "ac"; "a" ]

let def_black = [ "abc"; "ac"; "b"; "ac"; "b"; "bc"; "ab"; "c"; "c" ]

let cut_if b = print_endline (if b then "Cut" else "Skip")

let () =
  let rec do_wires red blue black =
    print_endline
      "Enter the wire colour (1 = red, 2 = blue, 3 = black) and connection (A, \
       B, C)";
    let line = String.lowercase (input_line stdin) in
    if String.length line = 2 then
      match line.[0] with
      | '1' ->
          cut_if (String.contains (List.hd red) line.[1]);
          do_wires (List.tl red) blue black
      | '2' ->
          cut_if (String.contains (List.hd blue) line.[1]);
          do_wires red (List.tl blue) black
      | '3' ->
          cut_if (String.contains (List.hd black) line.[1]);
          do_wires red blue (List.tl black)
      | _   -> do_wires red blue black
    else do_wires red blue black
  in
  do_wires def_red def_blue def_black

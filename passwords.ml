open Batteries

let passwords = File.lines_of "passwords.txt" |> List.of_enum

let read_lists =
  IO.lines_of stdin
  |> Enum.filter (( <> ) "")
  |> Enum.scanl (fun acc line -> line :: acc) []

let print_list ls = Printf.printf "[%s]\n%!" (String.concat ", " ls)

let filter_passwords filters =
  List.fold_righti
    (fun i input acc -> List.filter (fun v -> String.contains input v.[i]) acc)
    (List.rev filters) passwords

let () = read_lists |> Enum.iter (filter_passwords %> print_list)

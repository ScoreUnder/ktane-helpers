open Batteries

let passwords = File.lines_of "passwords.txt" |> List.of_enum

let read_lists f =
  let rec aux acc =
    let line = try input_line stdin with End_of_file -> "" in
    if line = "" then acc
    else
      let next = line :: acc in
      f next;
      aux next
  in
  aux []

let print_list ls = Printf.printf "[%s]\n%!" (String.concat ", " ls)

let filter_passwords filters =
  List.fold_righti
    (fun i input acc -> List.filter (fun v -> String.contains input v.[i]) acc)
    (List.rev filters) passwords

let () = read_lists (fun ls -> print_list (filter_passwords ls)) |> ignore

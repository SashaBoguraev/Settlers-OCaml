open Game

(* read-eval-print loop *)
let rec repl (eval : string -> string) : unit =
  print_string "> ";
  let input = read_line () in
  match input with
  | "" -> print_endline "bye"
  | _ -> input |> eval |> print_endline;
         repl eval

let () =
  print_endline "\n\nWelcome to Catan!\n";
  print_endline "           / \\     / \\";
  print_endline "          /   \\   /   \\";
  print_endline "         /     \\ /     \\";
  print_endline "        | Sheep | Wood  | ";
  print_endline "        |   6   |   4   | ";
  print_endline "       / \\     / \\     / \\";
  print_endline "      /   \\   /   \\   /   \\";
  print_endline "     /     \\ /     \\ /     \\";
  print_endline "    | Clay  | Desert|  Wood |";
  print_endline "    |   3   |       |   2   |";
  print_endline "    |   3   |       |   2   |";
  print_endline "     \\     / \\     / \\     /";
  print_endline "      \\   /   \\   /   \\   /";
  print_endline "       \\ /     \\ /     \\ /";
  print_endline "        | Wheat |  Ore  | ";
  print_endline "        |   5   |   1   | ";
  print_endline "         \\     / \\     / ";
  print_endline "          \\   /   \\   / ";
  print_endline "           \\ /     \\ / \n";










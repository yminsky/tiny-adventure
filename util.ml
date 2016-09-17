open Base

let printf = Printf.printf
let st s = String.strip s

let sayf format =
  Printf.ksprintf (fun s -> print_endline (st s)) format

let prompt () =
  printf "\n> %!";
  match input_line stdin with
  | exception _ -> Other []
  | x ->
    let words = 
      let open List.Let_syntax in
      String.split x ~on:' '
      >>| String.strip
      >>| String.lowercase
      >>| String.filter ~f:Char.is_alphanum
    in
    Parser.run words

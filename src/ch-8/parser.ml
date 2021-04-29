module Lexer = Lexer.Make (Lexer.Keyword) (Basic)

let () =
  let test_list = [ 4; 11; 1; 32; 4000; 100; -300; 11] in
  Basic.minl test_list
  |> Option.iter (fun x -> Printf.printf "Min: %d\n\n" x);

  Basic.maxl test_list
  |> Option.iter (fun x -> Printf.printf "Max: %d\n\n" x);

  ignore Lexer.scan

module Keyword : Types.KEYWORD = struct
  let alphas = []

  let symbols = []
end

module Lexer = Lexer.Make (Keyword) (Basic)

module Parser = Parser.Make (Lexer)

let () =
  let test_list = [ 4; 11; 1; 32; 4000; 100; -300; 11] in
  Basic.minl test_list
  |> Option.iter (fun x -> Printf.printf "Min: %d\n\n" x);

  Basic.maxl test_list
  |> Option.iter (fun x -> Printf.printf "Max: %d\n\n" x);

  Lexer.scan "The farmer's market opens at 8AM and closes at 3PM"
  |> List.map
    (fun token ->
       match token with
       | Lexer.Id id -> "id:" ^ id
       | Lexer.Key key -> "key:" ^ key)
  |> String.concat " "
  |> Printf.printf "Scanned: %s\n\n"
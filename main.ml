let () =
  if Array.length Sys.argv != 2 then
    print_endline "Supply expression as an argument"
  else
    match Parser.parse_sexp Sys.argv.(1) with
    | Some (input', output) ->
       Printf.printf "%i\n" output;
       if String.length input' > 0 then
         Printf.printf "Left over: `%s`\n" input';
    | None ->
      Printf.printf "Error";

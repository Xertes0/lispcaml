type 'a parser = string -> (string * 'a) option

let map (f: 'a -> 'b) (p: 'a parser): 'b parser =
  fun input ->
  match p input with
  | Some (input', output) -> Some (input', f output)
  | None -> None

let ( *> ) (p1: 'a parser) (p2: 'b parser): 'b parser =
      fun input ->
      match p1 input with
      | Some (input', _) -> p2 input'
      | None -> None

let ( <* ) (p1: 'a parser) (p2: 'b parser): 'a parser =
      fun input ->
      match p1 input with
      | Some (input', output) ->
         map (fun _ -> output) p2 input'
      | None -> None

let parse_char (c: char): char parser =
  fun input ->
  if String.starts_with ~prefix:(Char.escaped c) input
  then Some (String.sub input 1 (String.length input - 1), c)
  else None

let parse_string (str: string): string parser =
  fun input ->
  if String.starts_with ~prefix:str input
  then
    let str_len = String.length str in
    Some (String.sub input str_len (String.length input - str_len), str)
  else None

let string_to_seq (str: string): char Seq.t =
  Seq.init (String.length str) (String.get str)

let parse_while (cond: char -> bool): string parser =
  fun input ->
  let seq = string_to_seq input in
  let took =
    Seq.take_while cond seq |> List.of_seq in
  let took_len = List.length took in
  let took' = String.init took_len (List.nth took) in
  Some (String.sub input took_len (String.length input - took_len), took')

let parse_ws: string parser =
  parse_while (( == ) ' ')

let is_number (c: char): bool =
  let code = Char.code c in
  code >= 48 && code <= 57

let parse_int: int parser =
  fun input ->
  match parse_while is_number input with
  | Some (input', output) -> if String.length output > 0
                             then Some (input', Stdlib.int_of_string output)
                             else None
  | None -> None

let parse_identifier: string parser =
  parse_while (fun x -> x != ' ' && x != '(' && x != ')')

type value =
  | Number of int

let parse_number: value parser = map (fun x -> Number x) parse_int

let parse_expression(* : value parser *) =
  parse_char '(' *> parse_ws *> parse_identifier <* parse_ws <* parse_char ')'

(* let parse_expression: math_value parser = *)

(* type expression = { fun_name : string; };; *)

(* let parse_expression: parser *)

(* let main () = *)
(*   let rec read_loop () = *)
(*     let line = read_line () in *)
(*     print_endline line; *)
(*     read_loop (); *)
(*   in *)
(*   read_loop ();; *)

(* let () = main ();; *)

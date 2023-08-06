type 'a parser = string -> (string * 'a) option

let ( >>= ) (p1: 'a parser) (f: 'a -> 'b parser): 'b parser =
  fun input ->
  match p1 input with
  | Some (input', output) -> f output input'
  | None -> None

let map (f: 'a -> 'b) (p: 'a parser): 'b parser =
  p >>= fun output input' -> Some (input', f output)

let ( <$> ) (f: 'a -> 'b) (p: 'a parser): 'b parser = map f p

let ( <*> ) (pf: ('a -> 'b) parser) (p: 'a parser): 'b parser =
  pf >>= fun output ->
  p >>= fun output' input'' ->
  Some (input'', output output')

let ( *> ) (p1: 'a parser) (p2: 'b parser): 'b parser =
  p1 >>= fun _ input' ->
  p2 input'

let ( <* ) (p1: 'a parser) (p2: 'b parser): 'a parser =
  p1 >>= fun output ->
  p2 >>= fun _ input'' ->
  Some (input'', output)

let ( <|> ) (p1: 'a parser) (p2: 'a parser): 'a parser =
  fun input ->
  let p1_out = p1 input in
  if Option.is_some p1_out
  then p1_out
  else p2 input

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

let parse_identifier: string parser =
  parse_while (fun x -> x != ' ' && x != '(' && x != ')')

type value = float

let is_number (c: char): bool =
  let code = Char.code c in
  code >= 48 && code <= 57 || c == '.'

let parse_value: value parser =
  parse_while is_number >>= fun output input' ->
  if String.length output > 0
  then Some (input', Stdlib.float_of_string output)
  else None

let resolve_identifier (name: string): (value list -> value) =
  match name with
  | "+" -> List.fold_left ( +. ) 0.
  | "-" -> List.fold_left ( -. ) 0.
  | "*" -> List.fold_left ( *. ) 1.
  | "/" -> List.fold_left ( /. ) 1.
  | "^" -> (fun [a;b] -> a ** b)
  | "sqrt" -> (fun [a] -> Float.sqrt a)
  | _ -> failwith "Not implemented"

let unlazy_parser p =
  fun input -> Lazy.force p input

let rec parse_sexp': value parser lazy_t =
  lazy ((fun name args -> (resolve_identifier name) args)
        <$> (parse_char '(' *> parse_ws *> parse_identifier)
        <*> parse_args <* parse_ws <* parse_char ')')
and parse_arg: value parser lazy_t =
  lazy (parse_ws *> (parse_value <|> unlazy_parser parse_sexp'))
and parse_args: value list parser =
  fun input ->
  let input' = ref input in
  let output = ref [] in
  let cond = ref true in
  while !cond do
    match Lazy.force parse_arg !input' with
    | Some (input'', output') ->
       input' := input'';
       output := List.append !output [output']
    | None -> cond := false;
  done;
  Some (!input', !output)

let parse_sexp = unlazy_parser parse_sexp'

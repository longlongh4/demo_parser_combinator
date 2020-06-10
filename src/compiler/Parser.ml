external pretty : string -> string = "pretty" [@@bs.module]

let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false

type input_t = string * int (** input & index *)
let take_char ((input, index): input_t): (string * input_t) = (Js.String.charAt index input, (input, index + 1))

module ParserResult = struct
  type 'a t =
    | Success of 'a * input_t
    | Fail of string * input_t

  let (>>=) v f: 'a t = match v with 
    | Success(x, i) -> f(x,i)
    | Fail(e, i) -> Fail(e, i)

  type node = {
    label: string;
    count: int option;
    classname: string option;
  }

  let serialize_node {label; count; classname} (content: string) = 
    let element = match classname with
      | Some(classname) -> {j|<$label class="$classname">$content</$label>|j}
      | None -> {j|<$label>$content</$label>|j} in
    match count with
    | Some(count) -> Js.String.concatMany (Array.make count element) ""
    | None -> element
  let rec serialize = function
    | [] -> ""
    | x :: [] -> serialize_node x ""
    | h :: t -> serialize_node h (serialize t)
end

open ParserResult

type 'a parser = input_t -> 'a ParserResult.t

let gen_char_parser (f: char -> bool) (desc: string): string parser = fun input -> match take_char input with
  | ("", _) -> Fail("not enough input", input)
  | (x, input) when f(x.[0]) -> Success(x, input)
  | _ -> Fail("not a valid " ^ desc, input)

let digit_parser = gen_char_parser is_digit "digit"
let char_parser = gen_char_parser is_alpha "alphabet"
let asterisk_parser = gen_char_parser (fun x -> if x == '*' then true else false) "asterisk"
let greater_parser = gen_char_parser (fun x -> if x == '>' then true else false) "greater"
let dot_parser = gen_char_parser (fun x -> if x == '.' then true else false) "dot"

let andThen (p1: 'a parser) (p2: 'b parser): ('a * 'b) parser = fun input -> 
  p1 input >>= fun (r1, i1) -> p2 i1 >>= fun (r2, i2) -> Success((r1, r2), i2)
let orElse (p1: 'a parser) (p2: 'a parser): 'a parser = fun input -> 
  match p1 input with Success(r1, i1) -> Success(r1, i1) | Fail _ -> p2 input
let many (p: 'a parser): 'a list parser = fun input -> 
  let rec loop acc input = match p input with
    | Success(r, i) -> loop (r :: acc) i
    | Fail _ -> (List.rev acc, input) in
  let (r, i) = loop [] input in
  Success(r, i)

let many1 p = andThen p @@ many p
let optional p = fun input -> match p input with 
  | Fail _ -> Success(None, input) 
  | Success(r, i) -> Success(Some r, i)

let number_parser :string parser = 
  fun input -> many1 digit_parser input >>= fun ((v, ls), i) -> Success(Js.String.concatMany (Array.of_list ls) v, i)

let label_parser :string parser =
  fun input -> many1 char_parser input >>= fun ((v, ls), i) -> Success(Js.String.concatMany (Array.of_list ls) v, i)

(* ignore left *)
let (>>.) p q = fun input -> match andThen p q input with | Success((_, r2), i) -> Success(r2, i) | Fail(e, i) -> Fail(e, i)
let (>>) = andThen
let classname_parser :string parser = dot_parser >>. label_parser
let count_parser :string parser = asterisk_parser >>. number_parser
let node_parser = label_parser >> (optional classname_parser) >> (optional count_parser)
let sub_node_parser = greater_parser >>. node_parser
let expr_parser = node_parser >> (many sub_node_parser)
let parse str =
  let input = (str, 0) in 
  let create_node ((label, classname), count) = {label; classname; count = Belt.Option.map count int_of_string} in
  match expr_parser input with
  | Success((node, node_list), _) -> node :: node_list |> List.map create_node |> serialize |> pretty
  | Fail(e, _) -> "Error:" ^ e

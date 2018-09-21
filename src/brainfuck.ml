let string_to_list str =
  let rec aux i acc =
    if i < 0
    then acc
    else aux (i - 1) (str.[i] :: acc)
  in
  aux (String.length str - 1) []

type statement =
  | NextCell (* > *)
  | PreviousCell (* < *)
  | IncCell (* + *)
  | DecCell (* - *)
  | PrintCell (* . *)
  | GetChar (* , *)
  | Loop of cmd_seq (* [ ... ] *)
  | Comment of char

and cmd_seq = statement list

type token = char
type tokens = token list

module Parser: sig
  exception SyntaxError of string
  val parse: string -> cmd_seq
end = struct
  exception SyntaxError of string

  let rec parse_loop (acc: cmd_seq) (tokens: tokens): cmd_seq * tokens =
    match tokens with
    | ']' :: xs -> List.rev acc, xs
    | _ :: _ as l ->
      let (cmd, rest) = parse_char l in
      parse_loop (cmd :: acc) rest
    | [] -> raise (SyntaxError "] not found")

  and parse_char: tokens -> statement * tokens = function
    | '>' :: xs -> NextCell, xs
    | '<' :: xs -> PreviousCell, xs
    | '+' :: xs -> IncCell, xs
    | '-' :: xs -> DecCell, xs
    | '.' :: xs -> PrintCell, xs
    | ',' :: xs -> GetChar, xs
    | '[' :: xs ->
      let (list, rest) = parse_loop [] xs in
      Loop list, rest
    | ch :: xs -> Comment ch, xs
    | [] -> raise (SyntaxError "Empty list")

  let rec parse_chars cmds tokens =
    match tokens with
    | [] -> List.rev cmds
    | _ ->
      let (statement, rest) = parse_char tokens in
      parse_chars (statement :: cmds) rest

  let parse (str: string): cmd_seq =
    let tokens = string_to_list str in
    parse_chars [] tokens
end

module Tape: sig
  type tape
  val make: unit -> tape
  val getVal: tape -> int
  val setVal: int -> tape -> tape
  val moveRight: tape -> tape
  val moveLeft: tape -> tape
end = struct
  type tape = Tape of int list * int * int list
  (*                   left       v      right *)

  let hdtl = function
    | [] -> (0, [])
    | x :: xs -> (x, xs)

  let make () = Tape ([], 0, [])
  let getVal (Tape (_, v, _)) = v
  let setVal v (Tape (l, _, r)) = Tape (l, v, r)
  let moveRight (Tape (l, v, rr)) =
    let (x, r) = hdtl rr in
    Tape (v :: l, x, r)
  let moveLeft (Tape (ll, v, r)) =
    let (x, l) = hdtl ll in
    Tape (l, x, v :: r)
end

module Interpreter: sig
  type print_fn = char -> unit
  type getchar_fn = unit -> char
  type fns = print_fn * getchar_fn
  val eval: fns -> cmd_seq -> unit
end = struct
  type print_fn = char -> unit
  type getchar_fn = unit -> char
  type fns = print_fn * getchar_fn

  let rec eval_statement ((print, getchar) as fns) tape =
    let open Tape in
    function
    | NextCell -> moveRight tape
    | PreviousCell -> moveLeft tape
    | IncCell -> setVal (getVal tape + 1) tape
    | DecCell -> setVal (getVal tape - 1) tape
    | PrintCell -> getVal tape |> char_of_int |> print; tape
    | GetChar -> setVal (getchar () |> int_of_char) tape
    | Loop cmds as st ->
      if getVal tape = 0
      then tape
      else eval_cmds fns tape cmds |> (fun tape -> eval_statement fns tape st)
    | Comment _ -> tape

  and eval_cmds (fns: fns) (tape: Tape.tape) (cmds: cmd_seq): Tape.tape =
    List.fold_left (eval_statement fns) tape cmds

  let eval fns list = eval_cmds fns (Tape.make ()) list |> ignore
end

let get_char () = input_char stdin

let interpret str = str
  |> Parser.parse
  |> Interpreter.eval (print_char, get_char)

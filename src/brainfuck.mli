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

module Parser: sig
  exception SyntaxError of string
  val parse: string -> cmd_seq
end

module Interpreter: sig
  type print_fn = char -> unit
  type getchar_fn = unit -> char
  type fns = print_fn * getchar_fn
  val eval: fns -> cmd_seq -> unit
end

val interpret: string -> unit

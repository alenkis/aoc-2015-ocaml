open Base
open Stdio

module type Utils = sig
  val get_input_for_day : int -> string list
end

module Utils : Utils = struct
  let stringify_day day =
    let s = Caml.string_of_int day in
    match String.length s with
    | 1 -> "0" ^ s
    | _ -> s

  let get_input_for_day day : string list =
    day
    |> stringify_day
    |> Printf.sprintf "resources/day%s.txt"
    |> In_channel.read_lines
end

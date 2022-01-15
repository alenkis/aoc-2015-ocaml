open Stdio
open Base

let input = "resources/day01.txt" |> In_channel.read_all |> String.to_list

let char_to_number c =
  match c with
  | '(' -> 1
  | ')' -> -1
  | _ -> 0

(* Part 1 answer *)
let result input =
  List.fold ~init:0 ~f:(fun total el -> total + char_to_number el) input

(* Part 2 answer *)
let rec find_first_negative_floor input total_floor position =
  match total_floor < 0 with
  | true -> position
  | false -> (
      match input with
      | [] -> position
      | hd :: tl ->
          find_first_negative_floor tl
            (total_floor + char_to_number hd)
            (position + 1))
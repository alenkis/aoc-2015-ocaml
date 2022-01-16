open Base
open Utils

let input =
  1
  |> Utils.get_resource
  |> List.hd
  |> Option.value ~default:""
  |> String.to_list

let char_to_number c =
  match c with
  | '(' -> 1
  | ')' -> -1
  | _ -> 0

(* Part 1 answer *)
let find_final_floor input =
  List.fold ~init:0 ~f:(fun total el -> total + char_to_number el) input

(* Part 2 answer *)
let rec find_first_negative_floor' input total_floor position =
  match total_floor < 0 with
  | true -> position
  | false -> (
      match input with
      | [] -> position
      | hd :: tl ->
          find_first_negative_floor' tl
            (total_floor + char_to_number hd)
            (position + 1))

let find_first_negative_floor input = find_first_negative_floor' input 0 0

open Base
open Utils

type dimension = int * int * int

let convert_to_dimensions (input : string list) : dimension =
  let dimension_values = List.map ~f:Caml.int_of_string input in
  match dimension_values with
  | [ l; w; h ] -> (l, w, h)
  | _ -> failwith "Incorrect input dimensions"

let split_dimensions (dimension_string : string) =
  dimension_string |> String.split_on_chars ~on:[ 'x' ] |> convert_to_dimensions

let sort_dimensions (l, w, h) = List.sort ~compare [ l; w; h ]

let get_smallest_sides (l, w, h) =
  match List.sort ~compare [ l; w; h ] with
  | [ fst; snd; _ ] -> (fst, snd)
  | _ -> failwith "Unexpected error. You should've expected that!"

let calculate_required_wrapping_area (l, w, h) =
  let lw = l * w in
  let wh = w * h in
  let hl = h * l in
  let smallest_side = min lw (min wh hl) in
  (2 * lw) + (2 * wh) + (2 * hl) + smallest_side

let sum_dimensions acc dimension =
  acc + calculate_required_wrapping_area dimension

(* Part 1 answer *)
let wrapping_paper =
  2
  |> Utils.get_input_for_day
  |> List.map ~f:split_dimensions
  |> List.fold ~init:0 ~f:sum_dimensions

let calculate_ribbon (l, w, h) =
  let f, s = get_smallest_sides (l, w, h) in
  (2 * f) + (2 * s) + (l * w * h)

let sum_ribbon acc dimension = acc + calculate_ribbon dimension

(* Part 1 answer *)
let ribbon =
  2
  |> Utils.get_input_for_day
  |> List.map ~f:split_dimensions
  |> List.fold ~init:0 ~f:sum_ribbon

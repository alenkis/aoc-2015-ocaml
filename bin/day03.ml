open Base
open Utils

type coordinate = { x : int; y : int }

let parse_direction c : coordinate =
  match c with
  | '>' -> { x = 1; y = 0 }
  | '<' -> { x = -1; y = 0 }
  | '^' -> { x = 0; y = 1 }
  | 'v' -> { x = 0; y = -1 }
  | _ -> failwith "Unknown direction, are you drunk?"

let is_coord_equal c1 c2 = c1.x = c2.x && c1.y = c2.y
let is_coord_present coord coords = List.mem coords coord ~equal:is_coord_equal

let remove_duplicates coords =
  List.fold ~init:[]
    ~f:(fun acc el ->
      if is_coord_present el acc then acc else List.append acc [ el ])
    coords

let step acc { x = next_x; y = next_y } =
  let { x = last_x; y = last_y } = acc |> List.last |> Option.value_exn in
  List.append acc [ { x = last_x + next_x; y = last_y + next_y } ]

let directions =
  3
  |> Utils.get_resource
  |> List.hd
  |> Option.value ~default:""
  |> String.to_list
  |> List.map ~f:parse_direction

(* Part 1 answer *)
let result =
  directions
  |> List.fold ~init:[ { x = 0; y = 0 } ] ~f:step
  |> remove_duplicates
  |> List.length

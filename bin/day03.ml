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

let step_with_2 acc { x = next_x; y = next_y } =
  match acc |> List.rev with
  | _ :: { x = second_to_last_x; y = second_to_last_y } :: _ ->
      acc @ [ { x = second_to_last_x + next_x; y = second_to_last_y + next_y } ]
  | _ -> acc

let get_result init step_fn =
  3
  |> Utils.get_input_for_day
  |> List.hd
  |> Option.value ~default:""
  |> String.to_list
  |> List.map ~f:parse_direction
  |> List.fold ~init ~f:step_fn
  |> remove_duplicates
  |> List.length

(* Part 1 answer *)
let result1 = get_result [ { x = 0; y = 0 } ] step

(* Part 2 answer *)
let result2 = get_result [ { x = 0; y = 0 }; { x = 0; y = 0 } ] step_with_2

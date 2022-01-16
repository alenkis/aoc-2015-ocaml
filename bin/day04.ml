open Base
open Utils

let input_hash =
  4 |> Utils.get_input_for_day |> List.hd |> Option.value ~default:""

let get_hex_for_n secret n =
  secret ^ Int.to_string n |> Caml.Digest.string |> Caml.Digest.to_hex

let has_n_leading_zeros zeros str =
  match String.substr_index str ~pattern:(Utils.repeat_string "0" zeros) with
  | None -> false
  | Some x -> if x = 0 then true else false

let find_hash_for_secret secret zeros =
  let rec find_hash' n =
    if n |> get_hex_for_n secret |> has_n_leading_zeros zeros then n
    else find_hash' (n + 1)
  in
  find_hash' 1

let result1 = find_hash_for_secret input_hash 5
let result2 = find_hash_for_secret input_hash 6

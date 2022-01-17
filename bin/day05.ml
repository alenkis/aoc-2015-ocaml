open Base
open Utils

let vowels = [ 'a'; 'e'; 'i'; 'o'; 'u' ]

let is_vowel (c : char) =
  List.mem ~equal:Bool.equal
    (vowels |> List.map ~f:(fun v -> Char.equal v c))
    true

let contains_3_vowels s =
  s
  |> String.to_list
  |> List.map ~f:is_vowel
  |> List.fold ~init:0 ~f:(fun acc el -> if el then acc + 1 else acc)
  |> Int.( <= ) 3

let rec contains_repeating_letter s =
  match String.to_list s with
  | [] -> false
  | fst :: snd :: rest ->
      if Char.equal fst snd then true
      else
        rest
        |> String.of_char_list
        |> ( ^ ) (String.make 1 snd)
        |> contains_repeating_letter
  | fst :: tl -> (
      match List.hd tl with
      | None -> false
      | Some t -> if Char.equal fst t then true else false)

let forbidden = [ "ab"; "cd"; "pq"; "xy" ]

let rec contains_forbidden_strings s =
  match String.to_list s with
  | [] -> false
  | hd :: tl -> (
      match List.hd tl with
      | None -> false
      | Some snd_val ->
          let is_forbidden =
            [ hd; snd_val ]
            |> String.of_char_list
            |> List.mem forbidden ~equal:String.equal
          in
          if is_forbidden then true
          else contains_forbidden_strings (String.of_char_list tl))

(* Rules for part 1 *)
let is_nice_string s =
  s |> contains_3_vowels
  && s |> contains_repeating_letter
  && s |> contains_forbidden_strings |> not

let input = Utils.get_input_for_day 5
let accumulate_true acc el = if el then acc + 1 else acc

(* Part 1 answer *)
let result1 =
  input
  |> List.map ~f:is_nice_string
  |> List.fold_left ~init:0 ~f:accumulate_true

let rec contains_non_overlapping_pair s =
  match String.to_list s with
  | fst :: snd :: rest ->
      let has_pattern =
        s
        |> String.substr_index_all
             ~pattern:(String.of_char_list [ fst; snd ])
             ~may_overlap:false
        |> List.length
        |> Int.( < ) 1
      in
      if has_pattern then true
      else
        contains_non_overlapping_pair (String.of_char_list (List.cons snd rest))
  | _ -> false

let rec contains_sandwiched_pair s =
  match String.to_list s with
  | fst :: snd :: trd :: rest ->
      if Char.equal fst trd && not (Char.equal fst snd) then true
      else
        rest
        |> List.cons trd
        |> List.cons snd
        |> String.of_char_list
        |> contains_sandwiched_pair
  | _ -> false

let is_very_nice_string s =
  s |> contains_non_overlapping_pair && s |> contains_sandwiched_pair

(* Part 2 answer *)
let result2 =
  input
  |> List.map ~f:is_very_nice_string
  |> List.fold_left ~init:0 ~f:accumulate_true

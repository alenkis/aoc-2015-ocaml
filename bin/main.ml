let print_solution_for_day n s1 s2 =
  Printf.sprintf "Day %d, Part 1: " n
  ^ s1
  ^ "\n"
  ^ Printf.sprintf "Day %d, Part 2: " n
  ^ s2

let () =
  print_endline
    (print_solution_for_day 1
       (Day01.input |> Day01.find_final_floor |> Int.to_string)
       (Day01.input |> Day01.find_first_negative_floor |> Int.to_string))

let print_solution_for_day n s1 s2 =
  Printf.sprintf "Day %d, Part 1: " n
  ^ s1
  ^ "\n"
  ^ Printf.sprintf "Day %d, Part 2: " n
  ^ s2
  ^ "\n"

let combined_results =
  print_solution_for_day 1
    (Day01.input |> Day01.find_final_floor |> Int.to_string)
    (Day01.input |> Day01.find_first_negative_floor |> Int.to_string)
  ^ print_solution_for_day 2
      (Int.to_string Day02.wrapping_paper)
      (Int.to_string Day02.ribbon)
  ^ print_solution_for_day 3
      (Int.to_string Day03.result1)
      (Int.to_string Day03.result2)
  ^ print_solution_for_day 4 "N/A (brute force)" "N/A (brute force)"
  (* (Int.to_string Day04.result1) *)
  (* (Int.to_string Day04.result2) *)
  ^ print_solution_for_day 5
      (Int.to_string Day05.result1)
      (Int.to_string Day05.result2)

let () = print_endline combined_results

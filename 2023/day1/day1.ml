open Base
open Stdio
open Re2

let number_value = function
| "one" | "1" -> 1
| "two" | "2" -> 2
| "three" | "3" -> 3
| "four" | "4" -> 4
| "five" | "5" -> 5
| "six" | "6" -> 6
| "seven" | "7" -> 7
| "eight" | "8" -> 8
| "nine" | "9" -> 9
| _ -> failwith "no match"

let word_values = "one|two|three|four|five|six|seven|eight|nine"
let number_values = "1|2|3|4|5|6|7|8|9"

let find_all_digits string part2 = 
  let regexp = Re2.create_exn ("(" ^ number_values ^ (if part2 then "|" ^ word_values else "") ^ ")") in
  let matches = Re2.find_all_exn regexp string in
  matches

let get_val string part2 =
  let matches = find_all_digits part2 string in
  let l = 
    match List.nth matches 0 with
    | Some m -> m
    | None -> failwith "uhoh"
  in
  let r = 
    match List.nth matches (List.length matches - 1) with
    | Some m -> m
    | None -> failwith "uhoh"
  in
  (10 * (number_value l)) + (number_value r)

let part1 strings =
  let rec string_loop strings acc = 
    match strings with
    | [] ->  acc
    | h :: t -> string_loop t (acc + (get_val false h))
  in
  string_loop strings 0

let part2 strings = 
  let rec string_loop strings acc = 
    match strings with
    | [] ->  acc
    | h :: t -> string_loop t (acc + (get_val true h))
  in
  string_loop strings 0

let solve filename = 
  let content = In_channel.read_all filename in

  content
  |> String.split_lines
  |> part1 
  |> printf "part1: %d\n";

  content
  |> String.split_lines
  |> part2
  |> printf "part2: %d\n"

let () = solve "input.txt"
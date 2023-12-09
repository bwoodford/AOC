open Base
open Stdio
open Re

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

let word_values = "one\|two\|three\|four\|five\|six\|seven\|eight\|nine"
let number_values = "1\|2\|3\|4\|5\|6\|7\|8\|9"

let find_digits string part2 = 
  let regex = Re.Str.regexp (number_values ^ (if part2 then "\|" ^ word_values else "")) in
  let _ = Re.Str.search_forward regex string 0 in
  let first = Re.Str.matched_string string in
  let _ = Re.Str.search_backward regex string (String.length string) in
  let last = Re.Str.matched_string string in
  [first;last]

let get_val string part2 =
  let matches = find_digits part2 string in
  let l = List.nth_exn matches 0 in
  let r = List.nth_exn matches 1 in
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
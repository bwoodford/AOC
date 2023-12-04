open Base
open Stdio

let word_value = function
| "one" | "1" -> 1
| "two" | "2" -> 2
| "three" | "3" -> 3
| "four" | "4" -> 4
| "five" | "5" -> 5
| "six" | "6" -> 6
| "seven" | "7" -> 7
| "eight" | "8" -> 8
| "nine" | "9" -> 9
| _ -> 0
  
let find_all_digits str = 
  let pattern = Re.compile (Re.str "(one|two|three|four|five|six|seven|eight|nine|1|2|3|4|5|6|7|8|9)" ) in
  let matches = Re.exec pattern str in
  matches


let rec find_cali_val chars = 
  match chars with
  | [] -> failwith "no digit found"
  | h :: t -> match h with
    | '0'..'9' -> h
    | _ -> find_cali_val t

let get_cali_val string =
  let l = find_cali_val (String.to_list string) in
  let r = find_cali_val (String.to_list_rev string) in
  let s = String.make 1 l ^ String.make 1 r in
  Int.of_string s

let part1 strings =
  let rec string_loop strings acc = 
    match strings with
    | [] ->  acc
    | h :: t -> string_loop t (acc + (get_cali_val h))
  in
  string_loop strings 0

let get_val string =
  let matches = find_all_digits string in
  let l = Re.Group.get matches 0 in
  let r = Re.Group.get matches (Re.Group.nb_groups matches) in
  10 * (word_value l) + (word_value r)

let part2 strings = 
  let rec string_loop strings acc = 
    match strings with
    | [] ->  acc
    | h :: t -> string_loop t (acc + (get_val h))
  in
  string_loop strings 0

let solve filename = 
  let content = In_channel.read_all filename in

  (*
  content
  |> String.split_lines
  |> part1 
  |> printf "part1: %d\n";
  *)

  content
  |> String.split_lines
  |> part2
  |> printf "part2: %d\n"


let () = solve "example2.txt"
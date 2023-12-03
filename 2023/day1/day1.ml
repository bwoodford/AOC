open Base
open Stdio

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
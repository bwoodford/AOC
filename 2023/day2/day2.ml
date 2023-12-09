open Base
open Stdio

let get_number string = 
  let regex = Re.Str.regexp "[0-9]+" in
  let _ = Re.Str.search_forward regex string 0 in
  Int.of_string (Re.Str.matched_string string)

let get_color string =
  let regex = Re.Str.regexp "\\(red\\|green\\|blue\\)" in
  let _ = Re.Str.search_forward regex string 0 in
  Re.Str.matched_string string

let valid_game string = 
  let game_split = String.split_on_chars string ~on:[','] in
  let rec game_iteration_loop lst valid = 
    match valid with
    | false -> false
    | true ->
      match lst with
      | [] -> valid
      | h :: t ->
        let validation = match get_color h with
        | "red" -> if (get_number h) <= 12 then true else false
        | "green" -> if (get_number h) <= 13 then true else false 
        | "blue" -> if (get_number h) <= 14 then true else false
        | _ -> failwith "unknown color"
        in
        game_iteration_loop t validation
  in
  game_iteration_loop game_split true

let get_answer string =
  let games_break = 
    String.split_on_chars string ~on:[':'; ';'] 
  in
  let game_number = 
    match List.nth games_break 0 with
    | Some game -> get_number game
    | None -> failwith "no game number"
  in
  let games = 
    match List.tl games_break with
    | Some g -> g
    | None -> failwith "no games"
  in
  let rec game_loop games valid =
    match valid with
    | false -> false
    | true ->
      match games with
      | [] -> valid 
      | h :: t -> 
        game_loop t (valid_game h)
  in
  if game_loop games true then game_number else 0

let part1 strings = 
  let rec string_loop strings acc = 
    match strings with
    | [] ->  acc
    | h :: t -> string_loop t (acc + (get_answer h))
  in
  string_loop strings 0

let part2 strings = 1 

let solve filename = 
  let content = In_channel.read_all filename in

  content
  |> String.split_lines
  |> part1 
  |> printf "part1: %d\n"

  (*
  content
  |> String.split_lines
  |> part2
  |> printf "part2: %d\n"
  *)

let () = solve "input.txt"
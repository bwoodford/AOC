open Base
open Stdio

type colors = {
  red: int;
  blue: int;
  green: int;
}

let get_number string = 
  let regex = Re.Str.regexp "[0-9]+" in
  let _ = Re.Str.search_forward regex string 0 in
  Int.of_string (Re.Str.matched_string string)

let get_color string =
  let regex = Re.Str.regexp "\(red\|green\|blue\)" in
  let _ = Re.Str.search_forward regex string 0 in
  Re.Str.matched_string string

let split_colors colors = 
  String.split_on_chars colors ~on:[',']

let valid_game string = 
  let game_split = split_colors string in
  let rec game_iteration_loop lst valid = 
    match valid with
    | false -> false
    | true ->
      match lst with
      | [] -> valid
      | h :: t ->
        let number = get_number h in
        let validation = match get_color h with
        | "red" when number <= 12 -> true
        | "green" when number <= 13 -> true
        | "blue" when number <= 14 -> true 
        | _ -> false
        in
        game_iteration_loop t validation
  in
  game_iteration_loop game_split true


let get_min_colors string colors = 
  let game_split = split_colors string in
  let rec game_iteration_loop lst colors = 
    match lst with
    | [] -> colors
    | h :: t ->
      let number = get_number h in
      let color = get_color h in
      let updated_colors =
        match color with
        | "red" when number >= colors.red -> { colors with red = number }
        | "green" when number >= colors.green -> { colors with green = number }
        | "blue" when number >= colors.blue -> { colors with blue = number }
        | _ -> colors
      in
      game_iteration_loop t updated_colors
  in
  game_iteration_loop game_split colors

let get_answer string part2 =
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

  if part2 then
    let rec game_loop games colors =
      match games with
      | [] -> colors 
      | h :: t -> 
        game_loop t (get_min_colors h colors)
    in
    let colors = game_loop games { red = 0; green = 0; blue = 0} in
    colors.green * colors.blue * colors.red

  else
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
    | h :: t -> string_loop t (acc + (get_answer h false))
  in
  string_loop strings 0

let part2 strings =
  let rec string_loop strings acc = 
    match strings with
    | [] ->  acc
    | h :: t -> string_loop t (acc + (get_answer h true))
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
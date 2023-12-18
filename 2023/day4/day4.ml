open Base
open Stdio

type card = {
  theirs: int list;
  ours: int list;
}

let rec get_game_numbers game pos games =
  let regex = Re.Str.regexp "\d+" in
  let _ = Re.Str.search_forward regex game pos in
  match String.length game with
  | 0 -> games
  | _ -> get_game_numbers game (Re.Str.match_end ()) (Int.of_string (Re.Str.matched_string game) :: games)

let parse lines = 
  let get_games game =
    let segs = String.split_on_chars game ~on:[':';'|'] in
    let their_game = List.nth_exn segs 1 in
    let our_game = List.nth_exn segs 2 in
    { theirs = get_game_numbers their_game 0 []; ours = get_game_numbers our_game 0 []}
  in
  List.map ~f:(get_games) lines


let part1 card = 1 

let part2 card = 1

let solve filename = 
  let card = 
    In_channel.read_all filename
    |> String.split_lines
    |> parse
  in
  part1 card |> printf "part1: %d\n";
  part2 card |> printf "part2: %d\n"

let () = solve "example1.txt"
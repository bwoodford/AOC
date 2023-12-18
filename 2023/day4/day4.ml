open Base
open Stdio

type card = {
  theirs: int list;
  ours: int list;
}

let rec get_game_numbers game pos games =
  match pos with
  | pos when pos >= (String.length game)-1 -> games
  | _ -> 
    let regex = Re.Str.regexp "[0-9]+" in
    let _ = Re.Str.search_forward regex game pos in
    get_game_numbers game (Re.Str.match_end ()) (Int.of_string (Re.Str.matched_string game) :: games)

let parse lines = 
  let get_games game =
    let segs = String.split_on_chars game ~on:[':';'|'] in
    let their_game = List.nth_exn segs 1 in
    let our_game = List.nth_exn segs 2 in
    { theirs = get_game_numbers their_game 0 []; ours = get_game_numbers our_game 0 []}
  in
  List.map ~f:(get_games) lines

let rec get_matches theirs ours matches =
  match theirs with
    | [] -> matches
    | h :: t -> 
      if List.exists ~f:(fun num -> num = h) ours then 
        get_matches t ours (matches + 1)
      else 
        get_matches t ours matches

let part1 cards =
  let get_total_matches acc card =
    let matches = get_matches card.theirs card.ours 0 in
    if matches > 0 then
      acc + 2 ** (matches - 1)
    else
      acc + 0
  in
  List.fold_left ~f:(get_total_matches) ~init:0 cards

let part2 card = 1

let solve filename = 
  let card = 
    In_channel.read_all filename
    |> String.split_lines
    |> parse
  in
  part1 card |> printf "part1: %d\n";
  part2 card |> printf "part2: %d\n"

let () = solve "input.txt"
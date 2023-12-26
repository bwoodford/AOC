open Base
open Stdio
open Re

type race = {
  mutable time: int;
  mutable distance: int;
}

let round_up a b =
  let result = Float.of_int a /. Float.of_int b in
  let rounded_result = ceil result in
  Int.of_float rounded_result

let rec get_numbers string pos numbers =
  match pos with
  | pos when pos >= (String.length string)-1 -> numbers
  | _ -> 
    let regex = Re.Str.regexp "[0-9]+" in
    let _ = Re.Str.search_forward regex string pos in
    get_numbers string (Re.Str.match_end ()) (Int.of_string (Re.Str.matched_string string) :: numbers)

let parse string = 
  let split_heading = Re.Str.split (Re.Str.regexp "Time:\|Distance:") string in
  let times = get_numbers (List.hd_exn split_heading) 0 [] in
  let distances = get_numbers (List.nth_exn split_heading 1) 0 [] in
  List.map2_exn ~f:(fun t d -> { time=t; distance=d }) times distances

let calc_races acc race = 
  let rec loop dec distance acc =
    let diff = race.time - dec in
    match (diff * dec) with
    | v when v <= distance -> acc
    | _ -> loop (dec-1) distance (acc+1)
  in
  (*let matches = loop (Int.round_up (race.time / 2) ~to_multiple_of:2) race.distance 0 in*)
  let matches = loop (round_up race.time 2) race.distance 0 in
  let return = match matches with
    | v when v % 2 = 1 -> 2 * v - 1
    | v -> 2 * v
  in
  printf "%d\n" return;
  acc * return

let part1 races = 
  List.fold_left races ~f:(calc_races) ~init:1

let part2 strings = 1 

let solve filename = 
  let races = 
    In_channel.read_all filename
    |> parse
  in
  part1 races |> printf "part1: %d\n";
  part2 races |> printf "part2: %d\n"

let () = solve "example1.txt"
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
  let t = Float.of_int race.time in
  let d = Float.of_int race.distance in
  (*
    t = travel time
    B = button hold time
    T = race time
    D = total distance
    ------------------
    t = T-B    
    D = T*B
    -----------------
    Quadratic formula:
    B^2 - T*B + D = 0
  *)
  let p1 = (t +. Float.sqrt(t *. t -. 4. *. d)) /. 2. in
  let p2 = (t -. Float.sqrt(t *. t -. 4. *. d)) /. 2. in
  acc * Int.of_float ((Float.round_up p1) -. (Float.round_down p2) -. 1.)

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

let () = solve "input.txt"
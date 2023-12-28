open Base
open Stdio
open Re

type card = {
  hand: string;
  bid: int;
}

let parse string = 
  let line = Re.Str.split (Re.Str.regexp " ") string in
  { hand = List.hd_exn line; bid = Int.of_string (List.last_exn line) }

let part1 strings = 1 

let part2 strings = 1 

let solve filename = 
  let cards = 
    In_channel.read_all filename 
    |> String.split_lines 
    |> List.map ~f:(parse) in

  cards
  |> part1 
  |> printf "part1: %d\n";

  cards
  |> part2
  |> printf "part2: %d\n"

let () = solve "example1.txt"

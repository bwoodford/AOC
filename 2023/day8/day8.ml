open Base
open Stdio

type map = {
  left: string;
  right: string;
}

type instructions = {
  mutable table: (string, map) Hashtbl.t ref;
  directions: char list
}

let parse input = 
  let lines = input |> String.split_lines in
  let directions = String.to_list (List.hd_exn lines) in
  let map_string = List.tl_exn lines in
  let table = Hashtbl.create (module String) in
  { directions; table = ref table }

let part1 strings = 1 

let part2 strings = 1 

let solve filename = 
  let instructions = 
    In_channel.read_all filename
    |> parse
  in

  instructions
  |> part1 
  |> printf "part1: %d\n";

  instructions
  |> part2
  |> printf "part2: %d\n"

let () = solve "example1.txt"

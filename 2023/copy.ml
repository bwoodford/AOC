open Base
open Stdio

let part1 strings = 1 

let part2 strings = 1 

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

let () = solve "example1.txt"
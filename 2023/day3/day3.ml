open Base
open Stdio

type number = {
  xmin: int;
  xmax: int;
  y: int;
  value: int;
}

type symbol = {
  x: int;
  y: int;
  value: char;
}

type schematic = {
  symbols: symbol list;
  numbers: number list;
}

let print_schematic schematic =
  List.iter schematic.symbols ~f:(fun symbol -> printf "Symbol | x:%d y:%d value:%c\n" symbol.x symbol.y symbol.value);
  List.iter schematic.numbers ~f:(fun number -> printf "Number | xmin:%d xmax:%d y:%d value:%d\n" number.xmin number.xmax number.y number.value)

let aggregate_schematics root schematic = 
  {
    symbols = root.symbols @ schematic.symbols;
    numbers = root.numbers @ schematic.numbers;
  }

let is_adjacent symbol (number:number) = 
  abs (number.y - symbol.y) <= 1 && number.xmin - 1 <= symbol.x && symbol.x <= number.xmax + 1

let find_numbers_and_symbols input =
  let rec numsym input num xmax y numbers symbols =
    let xmax = xmax + 1 in
    match input with
    | ('0'..'9' as i1) :: ('0'..'9' as i2) :: t -> 
      numsym (i2 :: t)  (i1 :: num) xmax y numbers symbols
    | ('0'..'9' as i) :: t -> 
      let num = String.of_char_list (List.rev (i :: num)) in
      let xmin = xmax - (String.length num) in
      let number = { xmin; xmax=xmax-1; y; value=Int.of_string num } in
      numsym t [] xmax y (number :: numbers) symbols
    | '.' :: t -> 
      numsym t num xmax y numbers symbols
    | value :: t -> 
      numsym t num xmax y numbers ({ x=xmax-1; y; value } :: symbols)
    | [] -> { numbers; symbols }
    in
  let schematics = List.mapi ~f:(fun y -> fun row -> numsym row [] 0 y [] []) input in
  List.fold_left ~f:(aggregate_schematics) ~init:{ symbols=[]; numbers=[] } schematics

let part1 schematic = 
  schematic.numbers |> 
  List.filter ~f:(fun (num: number) -> schematic.symbols |> List.exists ~f:(fun (sym: symbol) -> is_adjacent sym num)) |>
  List.map ~f:(fun (num: number) -> num.value) |>
  List.fold_left ~f:(+) ~init:0

let part2 schematic =
  List.fold_left ~f:(fun acc sym ->
    match sym.value with
    | '*' ->
      let numbers = List.filter ~f:(fun num -> is_adjacent sym num) schematic.numbers in
      acc + if List.length numbers = 2 then (List.nth_exn numbers 0).value * (List.nth_exn numbers 1).value else 0
    | _ -> acc
  ) ~init:0 schematic.symbols
  

let solve filename = 
  let schematic = In_channel.read_lines filename |>
    List.map ~f:String.to_list |>
    find_numbers_and_symbols
  in
  let answer1 = part1 schematic in 
  printf "part1: %d\n" answer1;

  let answer2 = part2 schematic in 
  printf "part2: %d\n" answer2

let () = solve "input.txt"
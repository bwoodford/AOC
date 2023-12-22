open Base
open Stdio
open Re

type map = {
  seeds: int list;
  seed_soil: (int, int) Hashtbl.t;
  seed_fertilizer: (int, int) Hashtbl.t;
  fertilizer_water: (int, int) Hashtbl.t;
  water_light: (int, int) Hashtbl.t;
  light_temperature: (int, int) Hashtbl.t;
  temperature_humidity: (int, int) Hashtbl.t;
  humidity_location: (int, int) Hashtbl.t;
}

let fill_map map input = 
  let split_map = Re.Str.split (Re.Str.regexp "\[\n\|:\]") input in
  List.iter split_map ~f:(printf "%s\n\n\n");
  map

let create_maps strings = 
  List.fold_left strings ~f:(fill_map) ~init:
    {
      seeds = []; 
      seed_soil = Hashtbl.create (module Int);
      seed_fertilizer = Hashtbl.create (module Int);
      fertilizer_water = Hashtbl.create (module Int);
      water_light = Hashtbl.create (module Int);
      light_temperature = Hashtbl.create (module Int);
      temperature_humidity = Hashtbl.create (module Int);
      humidity_location = Hashtbl.create (module Int);
    }
  

let part1 strings = 1 

let part2 strings = 1 

let split_on_empty_lines input =
  Re.Str.split (Re.Str.regexp "^\n*$") input

let solve filename = 
  let maps = 
    In_channel.read_all filename
    |> split_on_empty_lines
    |> create_maps
  in
  ()

  (*
  part1 card |> printf "part1: %d\n";
  part2 card |> printf "part2: %d\n"
  *)

let () = solve "example1.txt"
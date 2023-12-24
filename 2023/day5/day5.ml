open Base
open Stdio
open Re

type map = {
  min_source: int;
  max_source: int;
  diff: int;
}

type maps = {
  mutable seeds: int list;
  mutable seed_soil: map list;
  mutable soil_fertilizer: map list;
  mutable fertilizer_water: map list;
  mutable water_light: map list;
  mutable light_temperature: map list;
  mutable temperature_humidity: map list;
  mutable humidity_location: map list;
}

let rec fill_maps maps entries = 
  match entries with
  | [] -> maps;
  | h :: t -> 
    let nums = List.map ~f:(Int.of_string) (Re.Str.split (Re.Str.regexp " ") h) in
    match nums with
    | d::s::r::_ -> 
      fill_maps ({ diff=d-s; min_source=s; max_source=s+r } :: maps) t
    | _ -> failwith "wrong number of items"

let load_maps maps input = 
  let split_map = Re.Str.split (Re.Str.regexp " map:\n\|:\n\|: \|\n") input in
  let () = 
    match List.hd_exn split_map with
    | "seed-to-soil" -> maps.seed_soil <- fill_maps maps.seed_soil (List.tl_exn split_map)
    | "soil-to-fertilizer" -> maps.soil_fertilizer <-fill_maps maps.soil_fertilizer (List.tl_exn split_map)
    | "fertilizer-to-water" -> maps.fertilizer_water <- fill_maps maps.fertilizer_water (List.tl_exn split_map)
    | "water-to-light" -> maps.water_light <- fill_maps maps.water_light (List.tl_exn split_map)
    | "light-to-temperature" -> maps.light_temperature <- fill_maps maps.light_temperature (List.tl_exn split_map)
    | "temperature-to-humidity" -> maps.temperature_humidity <- fill_maps maps.temperature_humidity (List.tl_exn split_map)
    | "humidity-to-location" -> maps.humidity_location <- fill_maps maps.humidity_location (List.tl_exn split_map)
    | _ -> maps.seeds <- List.map ~f:(Int.of_string) (Re.Str.split (Re.Str.regexp " ") (List.last_exn split_map))
  in
  maps

let create_maps strings = 
  List.fold_left strings ~f:(load_maps) ~init:
    {
      seeds = []; 
      seed_soil = [];
      soil_fertilizer = [];
      fertilizer_water = [];
      water_light = [];
      light_temperature = [];
      temperature_humidity = [];
      humidity_location = [];
    }

let find_range maps source = 
  let rec loop maps source dest =
    match maps with
    | []  -> dest
    | h :: t -> 
      if h.min_source <= source && h.max_source >= source then
        loop t source (source + h.diff)
      else
        loop t source dest
  in
  let dest = loop maps source 0 in
  if dest = 0 then source else dest

let find_seed_location maps seed acc =
  let soil = find_range maps.seed_soil seed in
  let fertilizer = find_range maps.soil_fertilizer soil in
  let water = find_range maps.fertilizer_water fertilizer in
  let light = find_range maps.water_light water in
  let temperature = find_range maps.light_temperature light in
  let humidity = find_range maps.temperature_humidity temperature in
  let location = find_range maps.humidity_location humidity in
  if acc > location then location else acc

let part1 maps = 
  List.fold_left maps.seeds ~f:(fun acc data -> find_seed_location maps data acc) ~init:Int.max_value

let part2 maps = 1 

let split_on_empty_lines input =
  Re.Str.split (Re.Str.regexp "^\n*$") input

let solve filename = 
  let maps = 
    In_channel.read_all filename
    |> split_on_empty_lines
    |> create_maps 
  in
  part1 maps |> printf "part1: %d\n";
  part2 maps |> printf "part2: %d\n"

let () = solve "input.txt"
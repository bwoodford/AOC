open Base
open Stdio
open Re

type maps = {
  mutable seeds: int list;
  seed_soil: (int, int) Hashtbl.t;
  soil_fertilizer: (int, int) Hashtbl.t;
  fertilizer_water: (int, int) Hashtbl.t;
  water_light: (int, int) Hashtbl.t;
  light_temperature: (int, int) Hashtbl.t;
  temperature_humidity: (int, int) Hashtbl.t;
  humidity_location: (int, int) Hashtbl.t;
}

let rec source_to_destination table d s r =
  match r with
  | 0 -> ()
  | _ -> 
    Hashtbl.add_exn table ~key:s ~data:d;
    source_to_destination table (d+1) (s+1) (r-1)

let rec fill_table table entries = 
  match entries with
  | [] -> ();
  | h :: t -> 
    let nums = List.map ~f:(Int.of_string) (Re.Str.split (Re.Str.regexp " ") h) in
    let () = match nums with
    | d::s::r::_ -> 
      source_to_destination table d s r;
    | _ -> failwith "wrong number of items"
    in
    fill_table table t

let load_map maps input = 
  let split_map = Re.Str.split (Re.Str.regexp " map:\n\|:\n\|: \|\n") input in
  let () = 
    match List.hd_exn split_map with
    | "seed-to-soil" -> fill_table maps.seed_soil (List.tl_exn split_map)
    | "soil-to-fertilizer" -> fill_table maps.soil_fertilizer (List.tl_exn split_map)
    | "fertilizer-to-water" -> fill_table maps.fertilizer_water (List.tl_exn split_map)
    | "water-to-light" -> fill_table maps.water_light (List.tl_exn split_map)
    | "light-to-temperature" -> fill_table maps.light_temperature (List.tl_exn split_map)
    | "temperature-to-humidity" -> fill_table maps.temperature_humidity (List.tl_exn split_map)
    | "humidity-to-location" -> fill_table maps.humidity_location (List.tl_exn split_map)
    | _ -> maps.seeds <- List.map ~f:(Int.of_string) (Re.Str.split (Re.Str.regexp " ") (List.last_exn split_map))
  in
  maps

let create_maps strings = 
  List.fold_left strings ~f:(load_map) ~init:
    {
      seeds = []; 
      seed_soil = Hashtbl.create (module Int);
      soil_fertilizer = Hashtbl.create (module Int);
      fertilizer_water = Hashtbl.create (module Int);
      water_light = Hashtbl.create (module Int);
      light_temperature = Hashtbl.create (module Int);
      temperature_humidity = Hashtbl.create (module Int);
      humidity_location = Hashtbl.create (module Int);
    }

let get_hash_value table value = 
  match (Hashtbl.find table value) with
  | Some v -> v
  | None -> value
  
let find_seed_location maps seed acc =
  let soil = get_hash_value maps.seed_soil seed in
  let fertilizer = get_hash_value maps.soil_fertilizer soil in
  let water = get_hash_value maps.fertilizer_water fertilizer in
  let light = get_hash_value maps.water_light water in
  let temperature = get_hash_value maps.light_temperature light in
  let humidity = get_hash_value maps.temperature_humidity temperature in
  let location = get_hash_value maps.humidity_location humidity in
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
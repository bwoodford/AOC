open Base
open Stdio
open Re

type card = {
  hand: char list;
  bid: int;
  groups: int list;
}

type state = {
  jokers: bool;
}

let current_state = ref { jokers = false }

let get_hand_weight hand =
  match hand with
  | '2' -> 1
  | '3' -> 2
  | '4' -> 3
  | '5' -> 4
  | '6' -> 5
  | '7' -> 6
  | '8' -> 7
  | '9' -> 8
  | 'T' -> 9
  | 'J' -> if current_state.contents.jokers then 0 else 10
  | 'Q' -> 11
  | 'K' -> 12
  | 'A' -> 13
  | _ -> failwith "Unexpected hand character"

let sort_order a b =
  match (a, b) with
  | (a,b) when a < b -> -1
  | (a,b) when a > b -> 1
  | _ -> 0

let add_or_set_value table ch =
  match Hashtbl.find table ch with
  | Some(value) -> Hashtbl.set table ~key:ch ~data:(value + 1)
  | None -> Hashtbl.add_exn table ~key:ch ~data:1

let create_hash_table hand =
  let table = Hashtbl.create (module Char) in
  List.fold hand ~f:(fun table ch -> add_or_set_value table ch; table) ~init:table

let table_to_sorted_list table =
  let table_list = Hashtbl.fold table ~f:(fun ~key:k ~data:v acc ->  v :: acc) ~init:[] in
  List.sort table_list ~compare:(fun a b -> if a > b then -1 else 1)

let parse string = 
  let line = Re.Str.split (Re.Str.regexp " ") string in
  let hand = String.to_list (List.hd_exn line) in
  let bid = Int.of_string (List.last_exn line) in
  let jokers = List.fold hand ~f:(fun acc data -> if Char.equal data 'J' then acc + 1 else acc) ~init:0 in
  let table = create_hash_table hand in
  if current_state.contents.jokers then
    Hashtbl.remove table 'J';
  let sorted_list = table_to_sorted_list table in
  let full_list = 
    match (List.length sorted_list) with
    | 0 -> [0]
    | other -> sorted_list
  in
  let groups = 
    match current_state.contents.jokers with
    | false -> full_list
    | true ->         
      let head = List.hd_exn full_list in
      let tail = 
        match List.tl full_list with
        | Some(tail) -> tail
        | None -> []
      in
      [(head + jokers)] @ tail
  in
  { hand; bid; groups }

let compare_hands a b =
  let rec loop a b = 
    match (a, b) with
    | ([], []) -> 0
    | (_::_, [] | [], _::_) -> failwith "lists must be same length"
    | (ah :: at, bh :: bt) -> 
      let hand_sort = 
        sort_order (get_hand_weight ah) (get_hand_weight bh) in
      match hand_sort with
      | 0 -> loop at bt
      | other -> other
  in
  loop a b

let rec compare_lists a b =
  match (a, b) with
  | ([], [])  -> 0
  | (_::_, [] | [], _::_) -> failwith "lists must be same length"
  | (ah :: at, bh :: bt) -> 
    let group_sort = sort_order ah bh in
    match group_sort with
    | 0 -> compare_lists at bt
    | other -> other
 
let compare_cards a b =
  let group_sort = 
    match (List.length a.groups, List.length b.groups) with
    | (x, y) when x > y -> -1
    | (x, y) when x < y -> 1
    | _ -> 0
    in
  let list_sort = 
    match group_sort with
    | 0 -> compare_lists a.groups b.groups
    | other -> other
    in
  match list_sort with
  | 0 -> compare_hands a.hand b.hand
  | other -> other

let part1 cards = 
  let sorted = List.sort cards ~compare:(compare_cards) in
  let rec loop cards i acc =
    match cards with
    | [] -> acc
    | h :: t -> loop t (i+1) (acc + i * h.bid)
  in
  loop sorted 1 0

let part2 cards =
  let sorted = List.sort cards ~compare:(compare_cards) in
  let rec loop cards i acc =
    match cards with
    | [] -> acc
    | h :: t -> loop t (i+1) (acc + i * h.bid)
  in
  loop sorted 1 0

let solve filename = 

  let file = 
    In_channel.read_all filename 
    |> String.split_lines in
    
  file 
  |> List.map ~f:(parse)
  |> part1 
  |> printf "part1: %d\n";

  current_state := {jokers = true};

  file 
  |> List.map ~f:(parse)
  |> part2
  |> printf "part2: %d\n"

let () = solve "input.txt"

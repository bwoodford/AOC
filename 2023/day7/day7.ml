open Base
open Stdio
open Re

type card = {
  hand: string;
  bid: int;
}

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
  | 'J' -> 10
  | 'Q' -> 11
  | 'K' -> 12
  | 'A' -> 13
  | _ -> failwith "unknown hand character"

let parse string = 
  let line = Re.Str.split (Re.Str.regexp " ") string in
  { hand = List.hd_exn line; bid = Int.of_string (List.last_exn line) }

let add_or_set_value table c =
  match Hashtbl.find table c with
  | Some(v) -> Hashtbl.set table ~key:c ~data:(v + 1)
  | None -> Hashtbl.add_exn table ~key:c ~data:1

let create_hash_table hand =
  let table = Hashtbl.create (module Char) in
  let rec loop hand i table =
    match String.length hand with
    | l when l = i -> ()
    | _ -> 
      add_or_set_value table hand.[i];
      loop hand (i+1) table
  in
  loop hand 0 table;
  table

let table_to_sorted_list table =
  let table_list = Hashtbl.fold table ~f:(fun ~key:k ~data:v acc ->  v :: acc) ~init:[] in
  List.sort table_list ~compare:(fun a b -> if a > b then -1 else 1)

let compare_strings a b =
  let rec loop ac bc = 
    match (ac, bc) with
    | ([], []) -> 0
    | (ac_h :: ac_t, bc_h :: bc_t) -> 
      begin
      match (get_hand_weight ac_h, get_hand_weight bc_h) with
      | (a, b) when a < b -> -1
      | (a, b) when a > b -> 1
      | (a, b) when a = b -> loop ac_t bc_t
      | _ -> failwith "lists must be the same size"
      end
    | _ -> failwith "lists must be same size"
  in
  loop a b

let rec compare_lists a b =
  match (a, b) with
  | ([], [])  -> 0
  | (ah :: at, bh :: bt) -> 
    begin
    match (ah, bh) with
    | (a,b) when a < b -> -1
    | (a,b) when a > b -> 1
    | _ -> compare_lists at bt 
    end
  | _ -> failwith "lists must be the same size"
 
let compare_cards a b =
  let a_table = create_hash_table a.hand in
  let b_table = create_hash_table b.hand in
  match (Hashtbl.length a_table, Hashtbl.length b_table) with
  | (x, y) when x > y -> -1
  | (x, y) when x < y -> 1
  | _ -> 
    let a_list = table_to_sorted_list a_table in
    let b_list = table_to_sorted_list b_table in
    match compare_lists a_list b_list with
    | 0 -> compare_strings (String.to_list a.hand) (String.to_list b.hand)
    | v -> v

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

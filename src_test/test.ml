
module Diff = Simple_diff.Make(String)
open Diff

let pp diffs =
  List.fold_left(fun str diff ->
    let content lines = String.concat "," (Array.to_list lines) in
    let new_str = match diff with
      | Equal lines ->
        "Equal: " ^ content lines
      | Deleted lines ->
        "Deleted: " ^ content lines
      | Added lines ->
        "Added: " ^ content lines
    in
    str ^ "\n" ^ new_str
  ) "" diffs

let () =
  let check old_lines new_lines expected =
    let diffs = get_diff old_lines new_lines in
    if diffs <> expected then
      let () = Printf.printf "FAILED!\n\nActual:%s" (pp diffs) 
      and () =      Printf.printf "\n\nExpected:%s" (pp expected) in
      exit 1
  in
  let () =
    let old_lines = [| "I"; "really"; "like"; "icecream" |]
    and new_lines = [| "I"; "do"; "not"; "like"; "icecream" |]
    and expected =
      [ Equal   [| "I"; |];
        Deleted [| "really" |];
        Added [| "do"; "not" |];
        Equal [| "like"; "icecream" |];
      ]
    in  check old_lines new_lines expected
  and () =
    let old_lines = [| "C"; "B"; "A" |]
    and new_lines = [| "A"; "B"; "C" |]
    and expected =
      [ Deleted [| "C" |];
        Added [| "A" |];
        Equal [| "B" |];
        Deleted [| "A" |];
        Added [| "C" |];
      ]
    in  check old_lines new_lines expected
  in
  Printf.printf "SUCCESS"


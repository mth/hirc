module H = Hashtbl
module S = String

let is_substr needle haystack =
  if needle = "" then true else
  let ch = needle.[0] and len = S.length needle and hlen = S.length haystack in
  let rec check from =
    if len > hlen - from then false else
    let pos = try S.index_from haystack from ch with Not_found -> -1 in
    pos >= 0 && pos + len <= hlen &&
      (S.sub haystack pos len = needle || check (pos + 1))
  in check 0

let read_base fn =
  let dict = H.create 1024 in
  let f = open_in fn in
  begin
    try
      while true do
        let line = input_line f in
        match S.split_on_char '\t' line with
        | [key; def; meta] ->
          let add = try
              let old, _, _ = H.find dict key in
              let len = S.length def and old_len = S.length old in
              if (len > old_len && def.[old_len] = ' ' || len = old_len) &&
                 S.sub def 0 old_len = old then
                Some (S.sub def old_len (len - old_len))
              else None
            with Not_found -> None in
          H.add dict key (def, add, meta)
        | _ -> failwith ("invalid input " ^ line)
      done
    with
      | End_of_file -> close_in f
      | ex -> close_in f; raise ex
  end;
  dict

let rec merge a b result =
  let cur_def = match result with
                | (def, _) :: _ -> def
                | [] -> "" in
  let add (def, add, meta) =
    match add with
    | Some add_str ->
      if is_substr add_str cur_def then result
      else (cur_def ^ add_str, meta) :: result
    | None ->
      if def <> cur_def then (def, meta) :: result
      else result in
  match a, b with
  | ((_, _, a_meta) as a_rec) :: a_tail,
    ((_, _, b_meta) as b_rec) :: b_tail ->
    if a_meta <= b_meta then
      merge a_tail b (add a_rec)
    else
      merge a b_tail (add b_rec)
  | a_rec :: a_tail, [] ->
    merge a_tail b (add a_rec)
  | [], b_rec :: b_tail ->
    merge a b_tail (add b_rec)
  | [], [] ->
    List.rev result

let drop_add = List.rev_map (fun (def, _, meta) -> def, meta)

let rev_merge = function
  | a, [] -> drop_add a
  | [], b -> drop_add b
  | a, b  -> merge (List.rev a) (List.rev b) []

let merge_dict dict_a dict_b =
  let all = H.create (H.length dict_a) in
  H.iter (fun key _ ->
    if not (H.mem all key) then
      H.add all key (rev_merge (H.find_all dict_a key, H.find_all dict_b key)))
    dict_a;
  H.iter (fun key _ ->
    if not (H.mem all key) then
      H.add all key (drop_add (H.find_all dict_b key))) dict_b;
  let keys = Array.make (H.length all) ""
  and merged = Array.make (H.length all) []
  and counter = ref 0 in
  H.iter (fun key values ->
    keys.(!counter) <- key;
    merged.(!counter) <- values;
    counter := !counter + 1) all;
  let merged_dict = Queue.create () in
  let rec collect () =
    let rec earliest nth result_time result =
      if nth >= Array.length merged then result else
      match merged.(nth) with
      | (_, time) :: _ when result < 0 || time < result_time ->
        earliest (nth + 1) time nth
      | _ -> earliest (nth + 1) result_time result in
    let idx = earliest 0 "" (-1) in
    if idx < 0 then merged_dict else
    match merged.(idx) with
    | (def, meta) :: tail ->
      Queue.add (keys.(idx), def, meta) merged_dict;
      merged.(idx) <- tail;
      collect ()
    | [] -> failwith "Unexpected empty list"
  in collect ()

let compare_dict what source merged =
  let merge_table = H.create (Queue.length merged) in
  Queue.iter (fun (key, def, meta) ->
    H.replace merge_table key def) merged;
  let source_table = H.create (H.length source) in
  H.iter (fun key (def, add_opt, meta) ->
    if not (H.mem source_table key) then
      H.add source_table key def) source;
  H.iter (fun key source_def ->
    try
      let merge_def = H.find merge_table key in
      if merge_def <> source_def then
        prerr_endline (key ^ " " ^ what ^ ": '" ^ source_def ^ "'\n"
        ^ S.make (S.length key) ' ' ^ " OUT: '" ^ merge_def ^ "'")
    with Not_found ->
      prerr_endline (key ^ " " ^ what ^ " missing: '" ^ source_def ^ "'"))
    source_table;

;;

(* Printexc.record_backtrace true; *)
if Array.length Sys.argv <= 1 then prerr_endline "Missing arguments" else
let dict_a = read_base Sys.argv.(1)
and dict_b = read_base Sys.argv.(2) in
let dict = merge_dict dict_a dict_b in

compare_dict "TUX" dict_a dict;
compare_dict "FUX" dict_b dict;
Queue.iter (fun (key, def, meta) ->
  Printf.printf "%s\t%s\t%s\n" key def meta) dict

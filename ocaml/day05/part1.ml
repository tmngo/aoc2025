(* 1.7 ms *)
Printf.printf "%i\n"
  (let lines = Util.read_lines "./day05/input" in
   let split_index =
     match List.find_index (fun line -> line = "") lines with
     | None -> failwith "unreachable"
     | Some idx -> idx
   in
   (* let raw_ranges = List.take split_index lines in *)
   (* let raw_ids = List.drop (split_index + 1) lines in *)
   (* let () = Printf.printf "frs: %s\n" (String.concat ", " raw_ranges) in *)
   (* let () = Printf.printf "ids: %s\n" (String.concat ", " raw_ids) in *)
   let ranges =
     List.map
       (fun line ->
         let min, max =
           match String.split_on_char '-' line with
           | a :: b :: _ -> (int_of_string a, int_of_string b)
           | _ -> failwith "unreachable"
         in
         (min, max))
       (List.take split_index lines)
   in
   List.fold_left
     (fun count id ->
       count
       + Bool.to_int (List.exists (fun (a, b) -> id >= a && id <= b) ranges))
     0
     (List.map (fun x -> int_of_string x) (List.drop (split_index + 1) lines)))

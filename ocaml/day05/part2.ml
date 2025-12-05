(* 1.5 ms *)
Printf.printf "%i\n"
  (let lines = Util.read_lines "./day05/input" in
   let split_index =
     match List.find_index (fun line -> line = "") lines with
     | None -> failwith "unreachable"
     | Some idx -> idx
   in
   let ranges =
     List.fold_left
       (fun acc (a, b) ->
         let overlapping, rest =
           List.partition (fun (ai, bi) -> ai <= b && bi >= a) acc
         in
         match overlapping with
         | [] -> (a, b) :: rest
         | _ ->
             List.fold_left
               (fun (amin, bmax) (ai, bi) -> (min ai amin, max bi bmax))
               (a, b) overlapping
             :: rest)
       []
       (List.map
          (fun line ->
            let min, max =
              match String.split_on_char '-' line with
              | a :: b :: _ -> (int_of_string a, int_of_string b)
              | _ -> failwith "unreachable"
            in
            (min, max))
          (List.take split_index lines))
   in
   (* let () =
     Printf.printf "frs: %s\n"
       (String.concat ", "
          (List.map
             (fun (a, b) -> string_of_int a ^ "-" ^ string_of_int b)
             ranges))
   in *)
   List.fold_left (fun count (a, b) -> count + b - a + 1) 0 ranges)

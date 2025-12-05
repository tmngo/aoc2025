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
         match acc with
         | (ai, bi) :: rest when a <= bi + 1 -> (ai, max bi b) :: rest
         | _ -> (a, b) :: acc)
       []
       (List.sort
          (fun (a1, _) (a2, _) -> a1 - a2)
          (List.map
             (fun line ->
               let min, max =
                 match String.split_on_char '-' line with
                 | a :: b :: _ -> (int_of_string a, int_of_string b)
                 | _ -> failwith "unreachable"
               in
               (min, max))
             (List.take split_index lines)))
   in
   (* let () =
     Printf.printf "frs: %s\n"
       (String.concat ", "
          (List.map
             (fun (a, b) -> string_of_int a ^ "-" ^ string_of_int b)
             ranges))
   in *)
   List.fold_left (fun count (a, b) -> count + b - a + 1) 0 ranges)

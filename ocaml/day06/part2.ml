(* 2.0 ms *)
Printf.printf "%i\n"
  (let lines = Array.of_list (Util.read_lines "./day06/input") in
   let indices =
     Array.of_seq
       (Seq.filter_map
          (fun (idx, char) ->
            match char with
            | '+' -> Some (idx, 0)
            | '*' -> Some (idx, 1)
            | _ -> None)
          (String.to_seqi lines.(Array.length lines - 1)))
   in
   let digit_rows =
     Array.map
       (fun row_idx ->
         let line = Array.get lines row_idx in
         Array.mapi
           (fun i (idx, _) ->
             let len =
               if i < Array.length indices - 1 then
                 fst (Array.get indices (i + 1)) - idx - 1
               else String.length line - idx
             in
             String.sub line idx len)
           indices)
       (Array.init (Array.length lines - 1) (fun i -> i))
   in
   (* let () =
     Printf.printf "%s\n"
       (String.concat "\n"
          (List.map
             (fun ls -> String.concat "|" (Array.to_list ls))
             (Array.to_list digit_rows)))
   in
   let () =
     Printf.printf "%s\n"
       (String.concat "|"
          (List.map
             (fun (idx, value) -> string_of_int idx ^ "," ^ string_of_int value)
             (Array.to_list indices)))
   in *)
   let numbers =
     Array.mapi
       (fun idx (_, op) ->
         let ncols = String.length digit_rows.(0).(idx) in
         let nrows = Array.length digit_rows in
         let cols =
           Array.init ncols (fun c ->
               int_of_string
                 (String.concat ""
                    (List.filter
                       (fun s -> s <> " ")
                       (List.init nrows (fun r ->
                            Char.escaped (String.get digit_rows.(r).(idx) c))))))
         in
         let value =
           Array.fold_left
             (if op = 0 then fun acc x -> acc + x else fun acc x -> acc * x)
             op cols
         in
         value)
       indices
   in
   (* let () = Printf.printf "%s\n" (String.concat "|" (Array.to_list numbers)) in *)
   Array.fold_left (fun acc x -> acc + x) 0 numbers)

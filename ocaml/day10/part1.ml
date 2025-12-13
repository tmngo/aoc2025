(* 4.4 ms *)
Printf.printf "%i\n"
  (let lines = Util.read_lines "./day10/input" in
   let machines =
     List.map
       (fun line ->
         let split = Array.of_list (String.split_on_char ' ' line) in
         let _, indicator_lights =
           String.fold_left
             (fun (i, lights) c ->
               match c with
               | '#' -> (i + 1, lights lor (1 lsl i))
               | '.' -> (i + 1, lights)
               | _ -> (i, lights))
             (0, 0) split.(0)
         in
         let buttons =
           Array.map
             (fun s ->
               List.fold_left
                 (fun acc digits -> acc lor (1 lsl int_of_string digits))
                 0
                 (String.split_on_char ','
                    (String.sub s 1 (String.length s - 2))))
             (Array.sub split 1 (Array.length split - 2))
         in
         (indicator_lights, buttons))
       lines
   in
   let rec check indicator_lights buttons idx selected =
     if
       indicator_lights
       = List.fold_left (fun acc x -> acc lxor buttons.(x)) 0 selected
     then
       (* let () =
         Printf.printf "[%s]\n"
           (String.concat "," (List.map string_of_int selected))
       in *)
       List.length selected
     else if idx = Array.length buttons then 9999
     else
       let a = check indicator_lights buttons (idx + 1) (idx :: selected) in
       let b = check indicator_lights buttons (idx + 1) selected in
       min a b
   in
   List.fold_left
     (fun acc (indicator_lights, buttons) ->
       acc + check indicator_lights buttons 0 [])
     0 machines)

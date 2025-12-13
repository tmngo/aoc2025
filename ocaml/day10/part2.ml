(* 3.9 ms *)
Printf.printf "%i\n"
  (let lines = Util.read_lines "./day10/example" in
   let machines =
     List.map
       (fun line ->
         let split = Array.of_list (String.split_on_char ' ' line) in
         let joltage_reqs_str = split.(Array.length split - 1) in
         let joltage_reqs =
           Array.of_list
             (List.map int_of_string
                (String.split_on_char ','
                   (String.sub joltage_reqs_str 1
                      (String.length joltage_reqs_str - 2))))
         in
         let place_values, _ =
           Array.fold_left
             (fun (place_values, acc) x ->
               let next = (x + 1) * acc in
               (next :: place_values, next))
             ([ 1 ], 1) joltage_reqs
         in
         let place_values = Array.of_list (List.rev place_values) in
         let () =
           Printf.printf "[%s]\n"
             (String.concat ","
                (List.map string_of_int (Array.to_list place_values)))
         in
         let buttons =
           Array.map
             (fun s ->
               List.map int_of_string
                 (String.split_on_char ','
                    (String.sub s 1 (String.length s - 2))))
             (Array.sub split 1 (Array.length split - 2))
         in
         (* let () =
           Array.sort (fun a b -> List.length b - List.length a) buttons
         in *)
         let buttons_num =
           Array.map
             (fun btn ->
               List.fold_left (fun acc i -> acc + place_values.(i)) 0 btn)
             buttons
         in
         let () =
           Printf.printf "[%s]\n"
             (String.concat ","
                (List.map string_of_int (Array.to_list buttons_num)))
         in
         (joltage_reqs, buttons))
       lines
   in

   List.fold_left
     (fun acc (joltage_reqs, buttons) ->
       let current_min = ref 9999 in
       let biggest_button =
         Array.fold_left (fun acc x -> max acc (List.length x)) 0 buttons
       in
       let rec check joltage_reqs buttons idx total_presses current_min =
         (* let () =
       Printf.printf "[%s]\n"
         (String.concat ","
            (List.map string_of_int (Array.to_list joltage_reqs)))
     in
     let arr = Array.copy joltage_reqs in
     let () =
       Printf.printf "buttons.(idx) [%s]\n"
         (String.concat "," (List.map string_of_int buttons.(idx)))
     in
     let () =
       List.iter (fun bi -> Array.set arr bi (arr.(bi) - presses)) buttons.(idx)
     in
     let () =
       Printf.printf "[%s]\n"
         (String.concat "," (List.map string_of_int (Array.to_list arr)))
     in *)
         if total_presses > !current_min then !current_min
         else if
           total_presses
           + (Array.fold_left ( + ) 0 joltage_reqs / biggest_button)
           > !current_min
         then !current_min (* else if total_presses + Array.fold_left *)
         else if Array.for_all (fun x -> x = 0) joltage_reqs then
           (* let () =
         Printf.printf "[%s]\n"
           (String.concat "," (List.map string_of_int selected))
       in *)
           let () = current_min := min !current_min total_presses in
           total_presses
         else if idx = Array.length buttons then 9999
         else
           let button = buttons.(idx) in
           let max_presses =
             List.fold_left
               (fun acc bi -> min acc joltage_reqs.(bi))
               9999 button
           in
           (* let () =
             Printf.printf "[%s]\n"
               (String.concat "," (List.map string_of_int button))
           in *)
           (* min
             (let arr = Array.copy joltage_reqs in
              let () =
                List.iter
                  (fun bi -> Array.set arr bi (arr.(bi) - max_presses))
                  button
              in
              check arr buttons (idx + 1)
                (total_presses + max_presses)
                current_min)
             (let arr = Array.copy joltage_reqs in
              let () =
                List.iter (fun bi -> Array.set arr bi (arr.(bi) - 0)) button
              in
              check arr buttons (idx + 1) (total_presses + 0) current_min) *)
           List.fold_left min 9999
             (List.init (max_presses + 1) (fun p ->
                  let arr = Array.copy joltage_reqs in
                  let () =
                    List.iter (fun bi -> Array.set arr bi (arr.(bi) - p)) button
                  in
                  check arr buttons (idx + 1) (total_presses + p) current_min))
       in
       acc + check joltage_reqs buttons 0 0 current_min)
     0 machines)

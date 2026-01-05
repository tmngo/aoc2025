(* 44.7 s *)
Printf.printf "%i\n"
  (let lines = Util.read_lines "./day10/input" in
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
         let buttons =
           Array.map
             (fun s ->
               List.map int_of_string
                 (String.split_on_char ','
                    (String.sub s 1 (String.length s - 2))))
             (Array.sub split 1 (Array.length split - 2))
         in
         (joltage_reqs, buttons))
       lines
   in

   List.fold_left
     (fun acc (joltage_reqs, buttons) ->
       let rec check joltage_reqs buttons idx =
         let is_even_before =
           Array.for_all (fun x -> x mod 2 = 0) joltage_reqs
         in
         if Array.for_all (fun x -> x = 0) joltage_reqs then 0
         else if Array.exists (fun x -> x < 0) joltage_reqs then 1000000
         else if idx = Array.length buttons then
           if is_even_before then
             2 * check (Array.map (fun x -> x / 2) joltage_reqs) buttons 0
           else 1000000
         else
           let joltage_reqs_1 = Array.copy joltage_reqs in
           let () =
             List.iter
               (fun bi -> Array.set joltage_reqs_1 bi (joltage_reqs_1.(bi) - 1))
               buttons.(idx)
           in
           (* let is_even_after =
             Array.for_all (fun x -> x mod 2 = 0) joltage_reqs_1
           in *)
           min
             (min
                (check joltage_reqs buttons (idx + 1))
                (if is_even_before then
                   2 * check (Array.map (fun x -> x / 2) joltage_reqs) buttons 0
                 else 1000000))
             (1 + check joltage_reqs_1 buttons (idx + 1))
       in
       acc + check joltage_reqs buttons 0)
     0 machines)

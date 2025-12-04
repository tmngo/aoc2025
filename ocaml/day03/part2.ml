(* 1.9 ms *)
Printf.printf "%i\n"
  (* sum over file *)
  (List.fold_left
     (fun total line ->
       let digits =
         (* sum over line for i in 11, 10, ... *)
         List.fold_left
           (fun (last_max_index, joltage) i ->
             (* max over line *)
             (* let () =
               Printf.printf "line: %i last_max_index: %i  i: %i\n"
                 (String.length line) last_max_index i
             in *)
             let a =
               List.fold_left
                 (fun (max_index, max_value) ci ->
                   let value = Char.code (String.get line ci) - 48 in
                   if value > max_value then (ci, value)
                   else (max_index, max_value))
                 (0, 0)
                 (let a = last_max_index + 1 in
                  let b = String.length line - 11 + i in
                  List.init (b - a) (fun x -> x + a))
             in
             (* let () =
               Printf.printf "max_index: %i  max_value: %i\n" (fst a) joltage
             in *)
             (fst a, joltage + (snd a * Util.pow 10 (11 - i))))
           (-1, 0)
           (List.init 12 (fun x -> x))
       in
       (* let () = Printf.printf "digits: %i\n" (snd digits) in *)
       total + snd digits)
     0
     (Util.read_lines "./day03/input"))

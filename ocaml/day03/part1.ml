(* 1.7 ms *)
Printf.printf "%i\n"
  (List.fold_left
     (fun total line ->
       (* i = 0 *)
       let max_tens_index, max_tens =
         List.fold_left
           (fun (max_ci, max_tens) ci ->
             let tens = Char.code (String.get line ci) - 48 in
             if tens > max_tens then (ci, tens) else (max_ci, max_tens))
           (0, 0)
           (let a = 0 in
            let b = String.length line - 1 in
            List.init (b - a) (fun x -> x + a))
       in
       (* i = 1 *)
       let _, max_ones =
         List.fold_left
           (fun (max_ones_index, max_ones) ci ->
             let ones = Char.code (String.get line ci) - 48 in
             if ones > max_ones then (ci, ones) else (max_ones_index, max_ones))
           (0, 0)
           (let a = max_tens_index + 1 in
            let b = String.length line - 0 in
            List.init (b - a) (fun x -> x + a))
       in
       (* let () = Printf.printf "%i %i %i\n" max_ci max_tens max_ones in *)
       total + (10 * max_tens) + max_ones)
     0
     (Util.read_lines "./day03/input"))

(* 3.6 ms *)
Printf.printf "%i\n"
  (let lines = Util.read_lines "./day04/input" in
   let dimx = String.length (List.hd lines) in
   let dimy = List.length lines in
   let matrix =
     Array.init_matrix dimy dimx (fun y x -> String.get (List.nth lines y) x)
   in
   let _, total =
     Array.fold_left
       (fun (y, total) row ->
         let _, line_total =
           Array.fold_left
             (fun (x, line_total) char ->
               let adjacent_count =
                 List.fold_left
                   (fun acc (dx, dy) ->
                     let xi = x + dx in
                     let yi = y + dy in
                     (* let () = Printf.printf "(%i %i) " xi yi in *)
                     if
                       xi >= 0 && xi < dimx && yi >= 0 && yi < dimy
                       && matrix.(yi).(xi) = '@'
                     then acc + 1
                     else acc)
                   0
                   [
                     (-1, -1);
                     (0, -1);
                     (1, -1);
                     (-1, 0);
                     (1, 0);
                     (-1, 1);
                     (0, 1);
                     (1, 1);
                   ]
               in

               (* let () =
                 Printf.printf "%s"
                   (if char = '@' then string_of_int adjacent_count else " ")
               in *)
               if char = '@' && adjacent_count < 4 then (x + 1, line_total + 1)
               else (x + 1, line_total))
             (0, 0) row
         in
         ( y + 1,
           (* let () = Printf.printf " = %i\n" line_total in *)
           total + line_total ))
       (0, 0) matrix
   in
   total)

(* 19.5 ms *)
Printf.printf "%i\n"
  (let lines = Util.read_lines "./day04/input" in
   let dimx = String.length (List.hd lines) in
   let dimy = List.length lines in
   let matrix =
     Array.init_matrix dimy dimx (fun y x ->
         (x, y, String.get (List.nth lines y) x))
   in
   let remove_rolls matrix =
     Array.fold_left
       (fun total row ->
         let line_total =
           Array.fold_left
             (fun line_total (x, y, c) ->
               let adjacent_count =
                 List.fold_left
                   (fun acc (dx, dy) ->
                     let xi = x + dx in
                     let yi = y + dy in
                     (* let () = Printf.printf "(%i %i) " xi yi in *)
                     if
                       xi >= 0 && xi < dimx && yi >= 0 && yi < dimy
                       &&
                       let _, _, ci = matrix.(yi).(xi) in
                       ci = '@'
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
                   (if c = '@' then string_of_int adjacent_count else " ")
               in *)
               if c = '@' && adjacent_count < 4 then (
                 (* *)
                 Array.set matrix.(y) x (x, y, 'X');
                 line_total + 1)
               else line_total)
             0 row
         in
         (* let () = Printf.printf " = %i\n" line_total in *)
         total + line_total)
       0 matrix
   in
   let rec fixed_point f initial_value state =
     let next_value = initial_value + f state in
     if next_value = initial_value then initial_value
     else fixed_point f next_value state
   in
   let removed = fixed_point remove_rolls 0 matrix in
   removed)

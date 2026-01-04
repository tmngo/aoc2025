(* 2.0 ms *)
Printf.printf "%i\n"
  (let lines = Util.read_lines "./day11/input" in
   let graph = Hashtbl.create 600 in
   let () =
     List.iter
       (fun line ->
         let key = String.sub line 0 3 in
         let values =
           String.split_on_char ' ' (String.sub line 5 (String.length line - 5))
         in
         Hashtbl.add graph key values)
       lines
   in
   let rec paths paths_out start dest =
     if start = dest then 1
     else
       match Hashtbl.find_opt paths_out start with
       | Some x -> x
       | None -> (
           match Hashtbl.find_opt graph start with
           | Some children ->
               List.fold_left
                 (fun acc child ->
                   let child_count = paths paths_out child dest in
                   let () = Hashtbl.add paths_out child child_count in
                   acc + child_count)
                 0 children
           | None -> 0)
   in
   let count_paths start dest = paths (Hashtbl.create 100) start dest in
   (count_paths "svr" "dac" * count_paths "dac" "fft" * count_paths "fft" "out")
   + count_paths "svr" "fft" * count_paths "fft" "dac" * count_paths "dac" "out")

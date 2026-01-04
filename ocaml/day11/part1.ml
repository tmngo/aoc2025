(* 1.5 ms *)
Printf.printf "%i\n"
  (let lines = Util.read_lines "./day11/input" in
   let graph = Hashtbl.create 600 in
   let paths_out = Hashtbl.create 600 in
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
   let rec count_paths start dest =
     if start = dest then 1
     else
       match Hashtbl.find_opt paths_out start with
       | Some x -> x
       | None ->
           let children = Hashtbl.find graph start in
           List.fold_left
             (fun acc child ->
               let child_count = count_paths child dest in
               let () = Hashtbl.add paths_out child child_count in
               acc + child_count)
             0 children
   in
   count_paths "you" "out")

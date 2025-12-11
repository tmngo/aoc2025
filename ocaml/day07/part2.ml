(* 3.1 ms *)
Printf.printf "%i\n"
  (let lines = Util.read_lines "./day07/input" in
   let dimx = String.length (List.hd lines) in
   let dimy = List.length lines in
   let matrix =
     Array.init_matrix dimx dimy (fun x y -> String.get (List.nth lines y) x)
   in
   let set = Hashtbl.create 1000 in
   let rec collect_splits worklist =
     match worklist with
     | [] -> 1
     | (x, y) :: rest ->
         let range = Seq.init (dimy - y) (fun yi -> y + yi) in
         (* let () = Printf.printf "%i, %i\n" x y in *)
         let search = Seq.find (fun yi -> matrix.(x).(yi) = '^') range in
         let value =
           match search with
           | Some yi -> (
               let id = x + (dimx * yi) in
               match Hashtbl.find_opt set id with
               | Some value -> value + collect_splits rest
               | None ->
                   let ct =
                     collect_splits [ (x - 1, yi + 1); (x + 1, yi + 1) ]
                   in
                   let () = Hashtbl.add set id ct in
                   ct + collect_splits rest)
           | None -> collect_splits rest
         in
         value
   in

   collect_splits [ (String.index (List.hd lines) 'S', 0) ])

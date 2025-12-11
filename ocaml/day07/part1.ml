(* 3.5 ms *)
module IntSet = Set.Make (Int);;

Printf.printf "%i\n"
  (let lines = Util.read_lines "./day07/input" in
   let dimx = String.length (List.hd lines) in
   let dimy = List.length lines in
   let matrix =
     Array.init_matrix dimx dimy (fun x y -> String.get (List.nth lines y) x)
   in
   let rec collect_splits worklist set =
     match worklist with
     | [] -> set
     | (x, y) :: rest -> (
         let range = Seq.init (dimy - y) (fun yi -> y + yi) in
         let () = Printf.printf "%i, %i\n" x y in
         let search = Seq.find (fun yi -> matrix.(x).(yi) = '^') range in
         match search with
         | Some yi ->
             let id = x + (dimx * yi) in
             if IntSet.mem id set then collect_splits rest set
             else
               collect_splits
                 ((x - 1, yi + 1) :: (x + 1, yi + 1) :: rest)
                 (IntSet.add id set)
         | None -> collect_splits rest set)
   in
   IntSet.cardinal
     (collect_splits [ (String.index (List.hd lines) 'S', 0) ] IntSet.empty))

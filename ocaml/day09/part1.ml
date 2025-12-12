(* 3.9 ms *)
module IntSet = Set.Make (Int);;

Printf.printf "%i\n"
  (let lines = Util.read_lines "./day09/input" in
   let points =
     List.map
       (fun s ->
         match String.split_on_char ',' s with
         | x :: y :: _ -> (int_of_string x, int_of_string y)
         | _ -> failwith "unreachable")
       lines
   in
   let rec get_distances points =
     match points with
     | [] -> []
     | pt :: rest ->
         List.append
           (List.map
              (fun i ->
                let px, py = pt in
                let ix, iy = i in
                (abs (px - ix) + 1) * (abs (py - iy) + 1))
              rest)
           (get_distances rest)
   in
   let result =
     List.fold_left
       (fun max_area area -> max area max_area)
       0 (get_distances points)
   in
   result)

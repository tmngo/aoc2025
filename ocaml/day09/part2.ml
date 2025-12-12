(* 51.2 ms *)
Printf.printf "%i\n"
  (let lines = Array.of_list (Util.read_lines "./day09/input") in
   let points =
     Array.map
       (fun s ->
         match String.split_on_char ',' s with
         | x :: y :: _ -> (int_of_string x, int_of_string y)
         | _ -> failwith "unreachable")
       lines
   in
   let edges =
     Array.mapi
       (fun i (x, y) ->
         let xnext, ynext =
           if i = Array.length points - 1 then points.(0) else points.(i + 1)
         in
         (min x xnext, min y ynext, max x xnext, max y ynext))
       points
   in
   let rec get_distances points =
     match points with
     | [] -> []
     | pt :: rest ->
         List.append
           (List.filter_map
              (fun i ->
                let px, py = pt in
                let ix, iy = i in
                let xmin = min px ix in
                let xmax = max px ix in
                let ymin = min py iy in
                let ymax = max py iy in
                let crosses_edge =
                  Array.exists
                    (fun (left, top, right, bottom) ->
                      right > xmin && left < xmax && top < ymax && bottom > ymin)
                    edges
                in
                if crosses_edge then None
                else Some ((xmax - xmin + 1) * (ymax - ymin + 1)))
              rest)
           (get_distances rest)
   in
   let result =
     List.fold_left
       (fun max_area area -> max area max_area)
       0
       (get_distances (Array.to_list points))
   in
   result)

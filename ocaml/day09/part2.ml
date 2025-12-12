(* 3.9 ms *)
module IntSet = Set.Make (Int);;

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
   let points =
     Array.mapi
       (fun i (x, y) ->
         let xprev, yprev =
           if i = 0 then points.(Array.length points - 1) else points.(i - 1)
         in
         let xnext, ynext =
           if i = Array.length points - 1 then points.(0) else points.(i + 1)
         in
         ( x,
           y,
           if x > xprev then if ynext > y then 0 else 1
           else if x < xprev then if ynext > y then 2 else 3
           else if y > yprev then if xnext > x then 4 else 5
           else if xnext > x then 6
           else 7 ))
       points
   in
   (* let () =
     Printf.printf "%s\n\n"
       (String.concat "\n"
          (List.map
             (fun (x, y, dir) ->
               string_of_int x ^ " " ^ string_of_int y ^ " " ^ string_of_int dir)
             (Array.to_list points)))
   in *)
   (* let _, signed_area =
     Array.fold_left
       (fun ((xprev, yprev), sum) (x, y, _) ->
         ((x, y), sum + ((x - xprev) * (y + yprev))))
       (let x0, y0, _ = Array.get points (Array.length points - 1) in
        ((x0, y0), 0))
       points
   in *)
   (* let is_clockwise = signed_area < 0 in *)
   (* let point_set =
     IntSet.of_list (List.map (fun (x, y) -> x + (100000 * y)) points)
   in *)
   (* let results = Dynarray.create in *)
   let rec get_distances points =
     match points with
     | [] -> []
     | pt :: rest ->
         List.append
           (List.filter_map
              (fun i ->
                let px, py, pdir = pt in
                let ix, iy, idir = i in
                let dx = ix - px in
                let dy = iy - py in
                let xmin = min px ix in
                let ymin = min py iy in
                let w = abs dx in
                let h = abs dy in
                let contains_point =
                  List.exists
                    (fun (x, y, _) ->
                      x > xmin && x < xmin + w && y > ymin && y < ymin + h)
                    points
                in
                let is_exterior dir dx dy =
                  match dir with
                  | 0 -> dx > 0 || dy < 0
                  | 1 -> dx < 0 && dy < 0
                  | 2 -> dx > 0 && dy > 0
                  | 3 -> dx < 0 || dy > 0
                  | 4 -> dx > 0 && dy < 0
                  | 5 -> dx > 0 || dy > 0
                  | 6 -> dx < 0 || dy < 0
                  | 7 -> dx < 0 && dy > 0
                  | _ -> failwith "unreachable"
                in
                let is_start_exterior = is_exterior pdir dx dy in
                let is_end_exterior = is_exterior idir (-dx) (-dy) in
                if contains_point || is_start_exterior || is_end_exterior then
                  None
                else
                  (* let () =
                    Printf.printf "(%i, %i), %i -> (%i, %i) -> %i\n" px py pdir
                      ix iy
                      ((w + 1) * (h + 1))
                  in *)
                  Some ((w + 1) * (h + 1)))
              rest)
           (get_distances rest)
   in
   let result =
     List.fold_left
       (fun max_area area -> max area max_area)
       0
       (get_distances (Array.to_list points))
   in
   (* let () = Printf.printf "signed_area: %i\n" signed_area in *)
   result)

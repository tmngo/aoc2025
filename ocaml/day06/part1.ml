(* 1.9 ms *)
Printf.printf "%i\n"
  (let lines = Array.of_list (Util.read_lines "./day06/input") in
   let split =
     Array.map
       (fun line ->
         Array.of_list
           (List.filter_map
              (fun s ->
                match s with
                | "" -> None
                | "*" -> Some 1
                | "+" -> Some 0
                | _ -> Some (int_of_string s))
              (String.split_on_char ' ' line)))
       lines
   in
   (* let () =
     Printf.printf "%s\n"
       (String.concat "\n"
          (List.map
             (fun ls ->
               String.concat " "
                 (List.map (fun x -> string_of_int x) (Array.to_list ls)))
             (Array.to_list split)))
   in *)
   let dimx = Array.length split.(0) in
   let dimy = Array.length lines in
   let matrix = Array.init_matrix dimx dimy (fun x y -> split.(y).(x)) in
   let result =
     Array.fold_left
       (fun acc col ->
         acc
         + Array.fold_left
             (if col.(dimy - 1) = 0 then fun sum row ->
                (* let () = Printf.printf "row: %i\n" row in *)
                sum + row
              else fun prod row -> prod * row)
             col.(dimy - 1)
             (Array.init dimy (fun i -> col.(i))))
       0
       (Array.init dimx (fun i -> matrix.(i)))
   in
   result)

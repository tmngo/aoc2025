(* 126.8 ms *)
module IntSet = Set.Make (Int);;

Printf.printf "%i\n"
  (let lines = Array.of_list (Util.read_lines "./day08/input") in
   let points =
     List.mapi
       (fun idx s ->
         match String.split_on_char ',' s with
         | x :: y :: z :: _ ->
             (idx, int_of_string x, int_of_string y, int_of_string z)
         | _ -> failwith "unreachable")
       (Array.to_list lines)
   in
   let rec get_distances points =
     match points with
     | [] -> []
     | pt :: rest ->
         List.append
           (List.map
              (fun i ->
                let pid, px, py, pz = pt in
                let iid, ix, iy, iz = i in
                ( pid,
                  iid,
                  ((px - ix) * (px - ix))
                  + ((py - iy) * (py - iy))
                  + ((pz - iz) * (pz - iz)) ))
              rest)
           (get_distances rest)
   in
   let sorted_distances =
     List.take 1000
       (List.sort (fun (_, _, a) (_, _, b) -> a - b) (get_distances points))
   in
   (* let () =
     Printf.printf "%s\n\n"
       (String.concat "\n"
          (List.map
             (fun (a, b, d) ->
               string_of_int a ^ " " ^ string_of_int b ^ " " ^ string_of_int d)
             sorted_distances))
   in *)
   let circuits = Hashtbl.create 200 in
   let sets = Hashtbl.create 200 in
   let rec loop sorted_distances =
     match sorted_distances with
     | [] -> ()
     | (a, b, _) :: rest ->
         let () =
           match (Hashtbl.find_opt circuits a, Hashtbl.find_opt circuits b) with
           | Some aa, Some bb ->
               if aa != bb then (
                 let a_set = Hashtbl.find sets aa in
                 let b_set = Hashtbl.find sets bb in
                 Hashtbl.replace_seq circuits
                   (Seq.product (IntSet.to_seq b_set) (Seq.singleton aa));
                 Hashtbl.replace sets aa (IntSet.union a_set b_set);
                 Hashtbl.remove sets bb)
           | Some aa, None ->
               Hashtbl.add circuits b aa;
               let prev = Hashtbl.find sets aa in
               Hashtbl.replace sets aa (IntSet.add b prev)
           | None, Some bb ->
               Hashtbl.add circuits a bb;
               let prev = Hashtbl.find sets bb in
               Hashtbl.replace sets bb (IntSet.add a prev)
           | None, None ->
               Hashtbl.add circuits a a;
               Hashtbl.add circuits b a;
               Hashtbl.replace sets a (IntSet.of_list [ a; b ])
         in
         loop rest
   in
   let () = loop sorted_distances in
   (* let () =
     Printf.printf "%s\n\n"
       (String.concat "\n"
          (List.map
             (fun (k, v) -> string_of_int k ^ ":  " ^ string_of_int v)
             (List.of_seq (Hashtbl.to_seq circuits))))
   in *)
   let values =
     List.sort
       (fun a b -> b - a)
       (List.map
          (fun k -> IntSet.cardinal (Hashtbl.find sets k))
          (List.of_seq (Hashtbl.to_seq_keys sets)))
   in
   (* let () =
     Printf.printf "values:\n%s\n"
       (String.concat "\n" (List.map (fun a -> string_of_int a) values))
   in *)
   let top = List.take 3 values in
   List.fold_left (fun acc x -> acc * x) 1 top)

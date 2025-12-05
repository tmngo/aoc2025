(* 99.3 ms *)
Printf.printf "%i\n"
  (let is_invalid x =
     let ndigits = int_of_float (log10 (float_of_int x)) + 1 in
     List.exists
       (fun n ->
         let first = x mod Util.pow 10 n in
         let rec check index =
           if index * n >= ndigits then true
           else
             let bi = x / Util.pow 10 (index * n) mod Util.pow 10 n in
             bi = first && check (index + 1)
         in
         check 1)
       (List.filter
          (fun n -> ndigits mod n == 0)
          (List.init (ndigits / 2) (fun x -> (ndigits / 2) - x)))
   in
   Array.fold_left
     (fun count s ->
       let min, max =
         match String.split_on_char '-' s with
         | a :: b :: _ -> (int_of_string a, int_of_string b)
         | _ -> failwith "unreachable"
       in
       (* let () = Printf.printf "======== %i-%i\n" min max in *)
       Array.fold_left
         (fun acc x -> acc + if is_invalid x then x else 0)
         count
         (Array.init (max - min + 1) (fun i -> i + min)))
     0
     (Array.of_list
        (String.split_on_char ',' (Util.read_whole_file "./day02/input"))))

(* 1.5 ms *)
Printf.printf "%i\n"
  (List.fold_left
     (fun count s ->
       let min, max =
         match String.split_on_char '-' s with
         | a :: b :: _ -> (int_of_string a, int_of_string b)
         | _ -> failwith "unreachable"
       in
       (* let () = Printf.printf "========= %i-%i\n" min max in *)
       count
       +
       let new_count = ref 0 in
       let x = ref min in
       while !x <= max do
         let ndigits = int_of_float (log10 (float_of_int !x)) + 1 in
         if ndigits mod 2 = 0 then (
           let half_pow = Util.pow 10 (ndigits / 2) in
           let left_half = !x / half_pow in
           let right_half = !x mod half_pow in
           (* let () = Printf.printf "%i %i\n" left_half right_half in *)
           new_count := !new_count + if left_half = right_half then !x else 0;

           x :=
             let newx =
               if left_half > right_half then (left_half * half_pow) + left_half
               else ((left_half + 1) * half_pow) + left_half + 1
             in
             newx)
         else x := Util.pow 10 ndigits + Util.pow 10 (ndigits / 2)
       done;
       !new_count)
     0
     (String.split_on_char ',' (Util.read_whole_file "./day02/input")))

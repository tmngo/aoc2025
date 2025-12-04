(* 1.5 ms *)
Printf.printf "%i\n"
  (snd
     (List.fold_left
        (fun (dial, password) s ->
          let delta =
            (if String.get s 0 = 'L' then -1 else 1)
            * int_of_string (String.sub s 1 (String.length s - 1))
          in
          let new_dial = (dial + 100 + delta) mod 100 in
          let new_password = password + if new_dial = 0 then 1 else 0 in
          (new_dial, new_password))
        (50, 0)
        (Util.read_lines "./day01/input")))

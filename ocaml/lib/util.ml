let read_whole_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let read_lines filename = String.split_on_char '\n' (read_whole_file filename)

let rec pow x n =
  if n < 0 then pow (1 / x) (-n)
  else if n = 0 then 1
  else if n mod 2 = 0 then pow (x * x) (n / 2)
  else x * pow (x * x) ((n - 1) / 2)

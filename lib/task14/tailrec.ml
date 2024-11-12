let collatz_length n =
  let rec aux n acc =
    if n = 1 then acc
    else if n mod 2 = 0 then aux (n / 2) (acc + 1)
    else aux (3 * n + 1) (acc + 1)
  in
  aux n 1

let longest_collatz limit =
  let rec aux current max_n max_len =
    if current >= limit then max_n
    else
      let len = collatz_length current in
      if len > max_len then aux (current + 1) current len
      else aux (current + 1) max_n max_len
  in
  aux 1 1 1

let solve limit = longest_collatz limit
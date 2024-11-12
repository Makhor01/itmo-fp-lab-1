module Collatz = struct
 (* Генерация последовательности Коллатца *)
 let rec sequence n =
  if n = 1 then [1]
  else if n mod 2 = 0 then n :: sequence (n / 2)
  else n :: sequence (3 * n + 1)

 (* Фильтрация - поиск максимальной длины последовательности *)
 let max_length limit =
  let rec aux n max_n max_len =
   if n > limit then (max_n, max_len)
   else
    let len = List.length (sequence n) in
    if len > max_len then aux (n + 1) n len
    else aux (n + 1) max_n max_len
  in
  aux 1 0 0
end

(* Свёртка - вычисление самой длинной последовательности *)
let longest_collatz_sequence limit =
 let (max_n, _) = Collatz.max_length limit in
 max_n

(* Основная функция *)
let solve limit = longest_collatz_sequence limit

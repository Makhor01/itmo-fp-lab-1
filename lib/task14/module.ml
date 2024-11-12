(* Модуль для работы с последовательностью Коллатца *)
module Collatz = struct
 (* Функция для генерации последовательности Коллатца *)
 let rec generate_sequence n =
  if n = 1 then [1]
  else n :: (if n mod 2 = 0 then generate_sequence (n / 2) else generate_sequence (3 * n + 1))

 (* Функция для вычисления длины последовательности *)
 let rec collatz_length n =
  if n = 1 then 1
  else 1 + (if n mod 2 = 0 then collatz_length (n / 2) else collatz_length (3 * n + 1))

 (* Функция для фильтрации последовательностей, завершившихся на 1 *)
 let filter_to_end_on_one seq =
  List.filter (fun x -> x = 1) seq

 (* Функция для свёртки и нахождения максимума *)
 let find_longest_sequence max_limit =
  let numbers = List.init max_limit (fun x -> x + 1) in
  List.fold_left
   (fun (max_len, max_num) i ->
    let length = collatz_length i in
    if length > max_len then (length, i) else (max_len, max_num))
   (0, 0)
   numbers

 (* Функция для решения задачи 14 *)
(* Функция для решения задачи 14 *)
let solve limit =
 let _, result = Collatz.find_longest_sequence limit in
 result


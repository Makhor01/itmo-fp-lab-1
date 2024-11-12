(* Бесконечная последовательность чисел от n *)
let rec infinite_seq n () = Seq.Cons (n, infinite_seq (n + 1))

(* Функция для вычисления длины последовательности Коллатца *)
let collatz_length n =
  let rec aux n acc =
    if n = 1 then acc
    else if n mod 2 = 0 then aux (n / 2) (acc + 1)
    else aux (3 * n + 1) (acc + 1)
  in
  aux n 1

(* Поиск числа с самой длинной последовательностью Коллатца, используя бесконечный поток *)
let longest_collatz_chain limit =
  infinite_seq 1
  (* Ограничиваем последовательность чисел до предела limit *)
  |> Seq.take_while (fun n -> n < limit)
  (* Преобразуем каждое число в кортеж (число, длина его последовательности Коллатца) *)
  |> Seq.map (fun n -> (n, collatz_length n))
  (* Находим максимальный элемент по длине последовательности *)
  |> Seq.fold_left (fun (max_n, max_len) (n, len) ->
       if len > max_len then (n, len) else (max_n, max_len))
     (1, 1)
  |> fst

(* Функция для решения задачи 14 *)
let solve limit = longest_collatz_chain limit
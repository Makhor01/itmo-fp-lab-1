(* Определение модуля с базовыми словами для чисел *)
module NumberWords = struct
  let ones = [| ""; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" |]
  let teens = [| "ten"; "eleven"; "twelve"; "thirteen"; "fourteen"; "fifteen"; "sixteen"; "seventeen"; "eighteen"; "nineteen" |]
  let tens = [| ""; ""; "twenty"; "thirty"; "forty"; "fifty"; "sixty"; "seventy"; "eighty"; "ninety" |]
  let hundred = "hundred"
  let thousand = "thousand"

  (* Преобразование числа в его текстовый эквивалент *)
  let rec number_to_words n =
    if n < 10 then ones.(n)
    else if n < 20 then teens.(n - 10)
    else if n < 100 then tens.(n / 10) ^ ones.(n mod 10)
    else if n < 1000 then
      let hundreds_part = ones.(n / 100) ^ hundred in
      let remainder_part = if n mod 100 = 0 then "" else "and" ^ number_to_words (n mod 100) in
      hundreds_part ^ remainder_part
    else if n = 1000 then ones.(1) ^ thousand
    else ""
end

(* Модуль для генерации последовательности чисел *)
module NumberSequence = struct
  (* Генерируем последовательность от 1 до max *)
  let generate max = Seq.unfold (fun n -> if n > max then None else Some (n, n + 1)) 1
end

(* Модуль для подсчета количества букв *)
module LetterCounter = struct
  open NumberWords

  (* Фильтрация последовательности - можно настроить *)
  let filter_numbers seq = seq  (* Пропускаем все числа, но можно изменить фильтр при необходимости *)

  (* Подсчет общего количества букв в текстовом представлении чисел *)
  let count_letters seq =
    seq
    |> Seq.map number_to_words   (* Преобразуем числа в текст *)
    |> Seq.map String.length     (* Считаем длины строк *)
    |> Seq.fold_left (+) 0       (* Суммируем длины *)
end

let solve limit =
  let sequence = NumberSequence.generate limit in     (* Генерация последовательности от 1 до limit *)
  let filtered_sequence = LetterCounter.filter_numbers sequence in  (* Фильтрация (если требуется) *)
  LetterCounter.count_letters filtered_sequence      (* Подсчет букв *)
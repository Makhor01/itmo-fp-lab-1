(* Определение слов для базовых чисел *)
let ones = [| ""; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" |]
let teens = [| "ten"; "eleven"; "twelve"; "thirteen"; "fourteen"; "fifteen"; "sixteen"; "seventeen"; "eighteen"; "nineteen" |]
let tens = [| ""; ""; "twenty"; "thirty"; "forty"; "fifty"; "sixty"; "seventy"; "eighty"; "ninety" |]
let hundred = "hundred"
let thousand = "thousand"

(* Функция для преобразования числа в его текстовый эквивалент *)
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

(* Функция для подсчета общего количества букв с использованием map *)
let count_letters max =
  List.init max (fun n -> n + 1)  (* Генерируем список чисел от 1 до max *)
  |> List.map number_to_words      (* Преобразуем каждое число в его текстовое представление *)
  |> List.map String.length        (* Получаем длину каждой строки *)
  |> List.fold_left (+) 0          (* Суммируем длины всех строк *)



let solve limit = count_letters limit 
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

(* Функция для подсчета количества букв в словесном представлении всех чисел от 1 до max *)
let rec count_letters n max =
  if n > max then 0
  else String.length (number_to_words n) + count_letters (n + 1) max

(* Запуск подсчета для чисел от 1 до 1000 *)
let solve limit = count_letters 1 limit 

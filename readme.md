## Хороше Максим Денисович P3325
# Лабораторная работа № 1

Цель: освоить базовые приёмы и абстракции функционального программирования: функции, поток управления и поток данных, сопоставление с образцом, рекурсия, свёртка, отображение, работа с функциями как с данными, списки.

##Задача 14: последовательно представлен код для всех требуемых вариантов реализации задачи.

Условие:
Последовательность Коллатца определяется для натуральных чисел следующим образом:
•	Если число n четное, то следующее число n/2.
•	Если число n нечетное, то следующее число 3n+1.
Такой процесс продолжается, пока не будет достигнуто значение 1. Например, для числа 13 последовательность выглядит так: 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
Таким образом, для числа 13 длина последовательности составляет 10 шагов.
 
## Решения

### хвостовая рекурсия – 
```Ocaml
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
```


### Модульной реализации
```Ocaml
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
```

 

## Задача 17

•	Необходимо подсчитать общее количество букв в написании всех чисел от 1 до 1000, игнорируя пробелы и дефисы.
•	•  Числа от 1 до 19 имеют свои уникальные названия (например, "one", "two", "three", ... "nineteen").
•	Числа десятков (20, 30, ..., 90) также имеют свои названия (например, "twenty", "thirty", ... "ninety").
•	Числа от 21 до 99 формируются как комбинация десятков и единиц (например, 21 = "twenty-one").
•	Числа от 100 до 999 представляют собой комбинации сотен и оставшихся чисел (например, 100 = "one hundred", 123 = "one hundred and twenty-three").
•	1000 представляется как "one thousand".
### Решение
### Обычная рекурсия
```Ocaml
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
```



### Хвостовая рекурсия 

```Ocaml
(* Функция для получения количества букв в написании чисел от 0 до 19 *)
let letters_1_to_19 n =
  match n with
  | 0 -> 0
  | 1 -> 3   (* one *)
  | 2 -> 3   (* two *)
  | 3 -> 5   (* three *)
  | 4 -> 4   (* four *)
  | 5 -> 4   (* five *)
  | 6 -> 3   (* six *)
  | 7 -> 5   (* seven *)
  | 8 -> 5   (* eight *)
  | 9 -> 4   (* nine *)
  | 10 -> 3  (* ten *)
  | 11 -> 6  (* eleven *)
  | 12 -> 6  (* twelve *)
  | 13 -> 8  (* thirteen *)
  | 14 -> 8  (* fourteen *)
  | 15 -> 7  (* fifteen *)
  | 16 -> 7  (* sixteen *)
  | 17 -> 9  (* seventeen *)
  | 18 -> 8  (* eighteen *)
  | 19 -> 8  (* nineteen *)
  | _ -> 0

(* Функция для получения количества букв в написании десятков *)
let letters_tens n =
  match n with
  | 2 -> 6   (* twenty *)
  | 3 -> 6   (* thirty *)
  | 4 -> 5   (* forty *)
  | 5 -> 5   (* fifty *)
  | 6 -> 5   (* sixty *)
  | 7 -> 7   (* seventy *)
  | 8 -> 6   (* eighty *)
  | 9 -> 6   (* ninety *)
  | _ -> 0

(* Основная рекурсивная функция для подсчета букв в написании чисел от 1 до n *)
let rec count_letters n =
  if n <= 0 then 0
  else if n < 20 then letters_1_to_19 n
  else if n < 100 then
    let tens = n / 10 in
    let units = n mod 10 in
    (letters_tens tens) + (letters_1_to_19 units)
  else if n < 1000 then
    let hundreds = n / 100 in
    let remainder = n mod 100 in
    let and_part = if remainder > 0 then 3 else 0 in  (* "and" has 3 letters *)
    (letters_1_to_19 hundreds) + 7 + and_part + (count_letters remainder)
  else if n = 1000 then 11  (* one thousand has 11 letters *)
  else 0

(* Функция для подсчета общего количества букв от 1 до 1000 *)
let total_letters () =
  let rec aux n acc =
    if n > 1000 then acc
    else aux (n + 1) (acc + count_letters n)
  in
  aux 1 0

(* Функция для решения задачи 14 *)
let solve limit =
  let rec aux n acc =
    if n > limit then acc
    else aux (n + 1) (acc + count_letters n)
  in
  aux 1 0
```

### Модульное решение
```Ocaml
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
```

 

### Работа с бесконечными списками
```Ocaml
(* Функция для преобразования числа в строку на английском языке *)
let number_to_words n =
  let ones = [| ""; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" |] in
  let teens = [| "ten"; "eleven"; "twelve"; "thirteen"; "fourteen"; "fifteen"; 
                 "sixteen"; "seventeen"; "eighteen"; "nineteen" |] in
  let tens = [| ""; ""; "twenty"; "thirty"; "forty"; "fifty"; 
                "sixty"; "seventy"; "eighty"; "ninety" |] in
  let hundred = "hundred" in
  let thousand = "thousand" in
  
  let aux n =
    if n = 0 then ""
    else if n < 10 then ones.(n)
    else if n < 20 then teens.(n - 10)
    else if n < 100 then tens.(n / 10) ^ ones.(n mod 10)
    else if n < 1000 then
      let hundreds_digit = n / 100 in
      let tens_digit = (n mod 100) / 10 in
      let ones_digit = n mod 10 in
      let last_two_digits = n mod 100 in
      let hundred_part =
        if hundreds_digit > 0 then ones.(hundreds_digit) ^ " " ^ hundred ^ (if last_two_digits > 0 then " and " else "")
        else "" in
      let tens_and_ones_part =
        if last_two_digits < 10 then ones.(last_two_digits)
        else if last_two_digits < 20 then teens.(last_two_digits - 10)
        else tens.(tens_digit) ^ ones.(ones_digit)
      in
      hundred_part ^ tens_and_ones_part
    else if n = 1000 then "one" ^ " " ^ thousand
    else "one" ^ " " ^ thousand  (* Для чисел больше 1000 можно добавить дополнительную логику *)
  in
  aux n

(* Модуль для работы с ленивыми списками *)
module LazyList = struct
  type 'a t = Cons of 'a * (unit -> 'a t)

  (* Бесконечный список от начального значения *)
  let rec from f n = Cons (f n, fun () -> from f (n + 1))

  (* Преобразование элементов списка *)
  let rec map f (Cons (x, next)) = Cons (f x, fun () -> map f (next ()))

  (* Взять первые n элементов *)
  let rec take n (Cons (x, next)) =
    if n = 0 then []
    else x :: take (n - 1) (next ())

  (* Свертка списка *)
  let rec fold_left f acc (Cons (x, next)) =
    fold_left f (f acc x) (next ())
end

(* Генерация бесконечного списка слов чисел *)
let number_words = LazyList.from (fun n -> number_to_words n) 1

(* Подсчет количества букв в первых n числах *)
let solve n =
  LazyList.take n number_words
  |> List.map (fun s -> String.length (String.concat "" (String.split_on_char ' ' s)))
  |> List.fold_left ( + ) 0
```

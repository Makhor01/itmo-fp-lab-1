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
(* Функция для вычисления длины последовательности Коллатца с использованием хвостовой рекурсии *)
let collatz_length n =
  let rec aux n length =
    if n = 1 then length
    else if n mod 2 = 0 then aux (n / 2) (length + 1)
    else aux (3 * n + 1) (length + 1)
  in
  aux n 1

(* Функция для поиска числа с самой длинной последовательностью Коллатца до миллиона *)
let find_longest_chain limit =
  let rec aux current max_number max_length =
    if current = limit then max_number
    else
      let length = collatz_length current in
      if length > max_length then
        aux (current + 1) current length
      else
        aux (current + 1) max_number max_length
  in
  aux 1 1 1

(* Поиск числа с самой длинной последовательностью для чисел от 1 до 1_000_000 *)
let result = find_longest_chain 1_000_000

let () =
(* Выводим результат *)
let () =  Printf.printf "Число, начинающее последовательность с самой длинной цепочкой: %d\n" result
```
Механизм работы:
Функция aux (вспомогательная функция) инициализируется вызовом aux n 1, где начальная длина равна 1.
Если n равно 1 (базовый случай), рекурсия завершается, и функция возвращает текущую длину length.
В остальных случаях проверяется, является ли n чётным или нечётным:
Если n чётное (n mod 2 = 0), следующая итерация вызывается с n / 2 и увеличенным значением length.
Если n нечётное, следующая итерация вызывается с 3 * n + 1 и увеличенным значением length.
Результат: функция возвращает длину последовательности Коллатца для начального значения n. 


### Обычная рекурсия
```Ocaml
(* Создаем хеш-таблицу для мемоизации *)
let memo = Hashtbl.create 10000

(* Добавляем начальное значение для числа 1 *)
let () = Hashtbl.add memo 1 1

(* Рекурсивная функция для вычисления длины цепочки Коллатца *)
let rec collatz_length n =
  if Hashtbl.mem memo n then Hashtbl.find memo n
  else
    let length =
      if n mod 2 = 0 then 1 + collatz_length (n / 2)
      else 1 + collatz_length (3 * n + 1)
    in
    Hashtbl.add memo n length;
    length

(* Функция для нахождения числа с максимальной цепочкой Коллатца *)
let compute () =
  let max_length = ref 0 in
  let number = ref 0 in
  for i = 1 to 1000000 do
    let length = collatz_length i in
    if length > !max_length then (
      max_length := length;
      number := i
    )
  done;
  !number

(* Выводим результат *)
let () =
  let result = compute () in
  Printf.printf "Число, начинающее последовательность с самой длинной цепочкой: %d\n" result
```
Механизм работы:
•	Сначала проверяется, существует ли значение для n в хеш-таблице с помощью Hashtbl.mem memo n. Если оно есть, возвращается ранее вычисленное значение через Hashtbl.find memo n.
•	Если значение отсутствует, происходит вычисление длины цепочки:
o	Если n чётное (n mod 2 = 0), вызывается collatz_length (n / 2), добавляя 1 к длине.
o	Если n нечётное, вызывается collatz_length (3 * n + 1), также добавляя 1.
•	После вычисления длины для n значение сохраняется в хеш-таблице с помощью Hashtbl.add memo n length.
•	Функция возвращает длину цепочки для текущего числа n.



### Модульной реализации
```Ocaml
(* Модуль для работы с последовательностью Коллатца *)
module Collatz = struct
  let memo = Hashtbl.create 10000

  (* Добавляем начальное значение для числа 1 *)
  let () = Hashtbl.add memo 1 1

  (* Функция для генерации последовательности Коллатца *)
  let rec generate_sequence n =
    if Hashtbl.mem memo n then []
    else
      let next_n = if n mod 2 = 0 then n / 2 else 3 * n + 1 in
      n :: generate_sequence next_n

  (* Функция для вычисления длины последовательности и мемоизации *)
  let rec collatz_length n =
    if Hashtbl.mem memo n then Hashtbl.find memo n
    else
      let length = 1 + collatz_length (if n mod 2 = 0 then n / 2 else 3 * n + 1) in
      Hashtbl.add memo n length;
      length

  (* Функция для фильтрации последовательностей, завершившихся на 1 *)
  let filter_to_end_on_one seq =
    List.filter (fun x -> Hashtbl.mem memo x) seq

  (* Функция для свёртки и нахождения максимума *)
  let find_longest_sequence max_limit =
    let numbers = List.init max_limit (fun x -> x + 1) in
    List.fold_left
      (fun (max_len, max_num) i ->
         let length = collatz_length i in
         if length > max_len then (length, i) else (max_len, max_num))
      (0, 0)
      numbers
end

let () =
  let _, result = Collatz.find_longest_sequence 1_000_000 in
  Printf.printf "Число, начинающее последовательность с самой длинной цепочкой: %d\n" result
```
Механизм работы:
•	Создаётся список чисел от 1 до max_limit с помощью List.init.
•	С помощью List.fold_left происходит итерация по каждому числу i:
o	Для каждого числа вызывается collatz_length i, чтобы получить его длину.
o	Если длина больше, чем текущая максимальная длина (max_len), обновляются max_len и max_num.
•	Функция возвращает кортеж, содержащий максимальную длину и соответствующее число.
 

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
let result = count_letters 1 1000
let () = Printf.printf "Total letters: %d\n" result

Механизм работы:
•	Если n больше max, возвращается 0 (рекурсивная база).
•	Иначе вычисляется длина строки, полученной с помощью number_to_words, и добавляется к результату рекурсивного вызова для следующего числа (n + 1).
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

(* Печатаем результат *)
let () =
  let result = total_letters () in
  Printf.printf "Total letters from 1 to 1000: %d\n" result
```
Механизм работы:
•	Если n больше 1000, возвращается накопленный счетчик.
•	Иначе вызывается рекурсивный вызов, увеличивая n на 1 и добавляя результат count_letters n к аккумулятору.
 
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

(* Запуск программы *)
let () =
  let sequence = NumberSequence.generate 1000 in     (* Генерация последовательности от 1 до 1000 *)
  let filtered_sequence = LetterCounter.filter_numbers sequence in  (* Фильтрация (если требуется) *)
  let result = LetterCounter.count_letters filtered_sequence in      (* Подсчет букв *)
  Printf.printf "Total letters: %d\n" result
```
Код организован в несколько отдельных функций, каждая из которых выполняет строго определённую задачу. Это способствует ясности и повторному использованию кода. Рассмотрим функции по порядку:
•	Функция letters_1_to_19:
o	Задача: возвращает количество букв для чисел от 0 до 19.
o	Реализация: использует match, что упрощает логику и делает её более читаемой.
•	Функция letters_tens:
o	Задача: возвращает количество букв для десятков (20, 30, ..., 90).
o	Реализация: также использует match, что поддерживает единообразие.
•	Функция count_letters:
o	Задача: вычисляет общее количество букв для произвольного числа n.
o	Реализация: включает в себя логику для обработки разных диапазонов чисел, используя другие функции, что делает её более компактной и понятной.
•	Функция total_letters:
o	Задача: суммирует количество букв для всех чисел от 1 до 1000.
o	Реализация: использует рекурсивный подход, что облегчает понимание общего процесса подсчёта.
 

### Работа с бесконечными списками
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

(* Функция для подсчета общего количества букв с использованием бесконечной последовательности *)
let count_letters max =
  Seq.unfold (fun n -> if n > max then None else Some (n, n + 1)) 1  (* Бесконечная последовательность от 1 до max *)
  |> Seq.map number_to_words                                         (* Преобразуем каждое число в его текстовое представление *)
  |> Seq.map String.length                                           (* Получаем длину каждой строки *)
  |> Seq.fold_left (+) 0                                             (* Суммируем длины всех строк *)

(* Запуск подсчета для чисел от 1 до 1000 *)
let result = count_letters 1000
let () = Printf.printf "Total letters: %d\n" result
```

Ленивая последовательность: Здесь используется Seq.unfold для генерации бесконечной последовательности от 1 до max. Это позволяет избежать создания большого списка заранее, что экономит память.
Функции высшего порядка:
•	Seq.map number_to_words: применяет функцию number_to_words ко всем элементам последовательности, преобразуя каждое число в текст.
•	Seq.map String.length: вычисляет длину каждой текстовой строки.
•	Seq.fold_left (+) 0: суммирует длины строк, возвращая общее количество букв.

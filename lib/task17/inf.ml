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

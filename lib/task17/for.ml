(* Функция для преобразования числа в строку на английском языке *)
let number_to_words n =
  let ones = [| ""; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" |] in
  let teens = [| "ten"; "eleven"; "twelve"; "thirteen"; "fourteen"; "fifteen"; 
                 "sixteen"; "seventeen"; "eighteen"; "nineteen" |] in
  let tens = [| ""; ""; "twenty"; "thirty"; "forty"; "fifty"; 
                "sixty"; "seventy"; "eighty"; "ninety" |] in
  let hundred = "hundred" in
  let thousand = "thousand" in
  if n = 1000 then "one" ^ thousand
  else
    let hundreds_digit = n / 100 in
    let tens_digit = (n mod 100) / 10 in
    let ones_digit = n mod 10 in
    let last_two_digits = n mod 100 in
    let hundred_part = 
      if hundreds_digit > 0 then ones.(hundreds_digit) ^ hundred ^ (if last_two_digits > 0 then "and" else "")
      else "" in
    let tens_and_ones_part =
      if last_two_digits < 10 then ones.(last_two_digits)
      else if last_two_digits < 20 then teens.(last_two_digits - 10)
      else tens.(tens_digit) ^ ones.(ones_digit)
    in
    hundred_part ^ tens_and_ones_part

(* Подсчет общего количества букв *)
let solve n =
  let total = ref 0 in
  for i = 1 to n do
    total := !total + String.length (number_to_words i)
  done;
  !total

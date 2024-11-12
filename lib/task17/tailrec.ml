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

let solve limit = total_letters () 

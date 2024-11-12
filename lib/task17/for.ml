(** [div_sum n] is sum of divisors of [n] calculated directly by bruteforcing
    all numbers under [n] and adding them to sum into variable [sum] *)
let div_sum n =
  let sum = ref 0 in
  for i = 1 to n - 1 do
    if n mod i == 0 then sum := !sum + i
  done;
  !sum

(** [solve max] finds sum of amicible numbers under [max]*)
let solve max =
  let sum = ref 0 in
  for i = 1 to max - 1 do
    (* when [i] and [j] are amicible, j = div_sum i and i = div_sum j *)
    let j = div_sum i in
    if i <> j && div_sum j = i then sum := !sum + i
  done;
  !sum

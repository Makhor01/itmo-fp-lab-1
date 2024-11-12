  (** [collatz_chain n] calculates the length of the Collatz chain for a given number [n] *)
let rec collatz_chain n =
 if n = 1 then 1
 else if n mod 2 = 0 then 1 + collatz_chain (n / 2)
 else 1 + collatz_chain (3 * n + 1)

(** [longest_collatz_chain limit] finds the largest number not exceeding [limit]
  that has the longest Collatz chain
  @return the largest number with the longest Collatz chain *)
let longest_collatz_chain limit =
 let longest_chain = ref 0 in
 let longest_start = ref 0 in
 for i = 1 to limit do
  let chain_length = collatz_chain i in
  if chain_length > !longest_chain then (
   longest_chain := chain_length;
   longest_start := i
  )
 done;
 !longest_start

(** [solve limit] finds the largest number not exceeding [limit]
  that has the longest Collatz chain
  @param limit the upper bound for the search
  @return the largest number with the longest Collatz chain *)
let solve limit = longest_collatz_chain limit
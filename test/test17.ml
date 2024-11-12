let task17_implementations =
  [
    ("Recursive algorithm", Task17.Rec.solve);
    ("Tail-recursive algorithm", Task17.Tailrec.solve);
    ("Module implementation", Task17.Module.solve);
    ("Map based implementation", Task17.Map.solve);
    ("For loop based implementation", Task17.For.solve);
  ]

let task17_checker solve_fun () =
  Alcotest.(check int) "Valid output" 21124 (solve_fun 1000)

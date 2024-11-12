let task14_implementations =
  [
    ("Tail-recursive algorithm", Task14.Tailrec.solve);
    ("Module implementation", Task14.Module.solve);
    ("For loop based implementation", Task14.For.solve);
    ("Infinite sequences based algorithm", Task14.Inf.solve);
  ]

let task14_checker solve_fun () =
  Alcotest.(check int) "Valid output" 837799 (solve_fun 1000000)

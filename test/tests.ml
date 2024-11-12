open Test14
open Test17

let testset_from_impls checker =
  List.map (fun (name, solve) -> Alcotest.test_case name `Quick (checker solve))

let report_path = Sys.getenv_opt "REPORT_PATH"

let () =
  Utils.run_with_save_report "euler" "Euler problems solutions test" report_path
    [
      ("Task #14", testset_from_impls task14_checker task14_implementations);
      ("Task #17", testset_from_impls task17_checker task17_implementations);
    ]

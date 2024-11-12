(lang dune 3.16)

(name lab1)

(generate_opam_files true)

(source
 (github Makhor01/itmo-fp-lab-1))

(authors "Maksim Khoroshev")

(maintainers "Maksim Khoroshev")

(license MIT)

(package
 (name lab1)
 (synopsis "ITMO FP course laboratory work #1")
 (description "Solutions for Euler project tasks 14 and 17 in OCaml")
 (depends ocaml dune (alcotest :with-test) (junit_alcotest :with-test))
 (allow_empty))

(lang dune 3.20)

(name part-ii-project-code)

(generate_opam_files true)

(source
 (github advay168/part-ii-project-code))

(authors "Advay Gupta")

(maintainers "Advay Gupta")

(license LICENSE)

(using menhir 3.0)

(package
 (name part-ii-project-code)
 (synopsis "Part II project code")
 (description "Cambridge Computer Science Tripos Part II Project")
 (depends
   (ocaml (>= 5.3))
   (base (>= 0.17))
   (cmdliner (>= 2.0))
   (menhir (>= 20250912))
   (ppx_jane (>= 0.17))
   (wcwidth (>= 1.0)))
 )

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html

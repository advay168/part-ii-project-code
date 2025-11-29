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
 (depends ocaml base cmdliner menhir ppx_jane)
 )

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html

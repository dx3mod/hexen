(lang dune 3.6)

(name hexen)

(generate_opam_files true)

(source
 (github dx3mod/hexen))

(authors "Mikhail Lopatin <dx3mod@bk.ru>")

(maintainers "Mikhail Lopatin <dx3mod@bk.ru>")

(license MIT)

(package
 (name hexen)
 (synopsis
  "Yet another library for encoding and decoding hexadecimal byte sequences")
 (depends
  (ocaml
   (>= 4.14))
  (alcotest :with-test))
 (tags
  (hex hexadecimal)))

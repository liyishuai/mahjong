(lang dune 3.13)

(name mahjong)

(generate_opam_files true)

(source
 (github liyishuai/mahjong))

(authors "Yishuai Li")

(maintainers "Yishuai Li")

(license MPL-2.0)

(package
 (name mahjong)
 (synopsis "Mahjong library for OCaml")
 (depends
  (ocaml (>= 5.0))
  dune
  (domainslib (>= 0.5.0))))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html

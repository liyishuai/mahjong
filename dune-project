(lang dune 3.0)
(name mahjong)
(using menhir 2.1)
(generate_opam_files true)

(package
 (name mahjong)
 (synopsis "Mahjong library ported from mjx")
 (description "A Mahjong library ported from mjx to OCaml, using Cap'n Proto for serialization/RPC.")
 (maintainers "Yishuai Li")
 (authors "Yishuai Li")
 (license MPL-2.0)
 (depends
  (dune (>= 2.9))
  (ocaml (>= 5.2))
  base
  stdio
  capnp
  capnp-rpc-lwt
  lwt
  (alcotest :with-test)
 )
)

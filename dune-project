(lang dune 3.4)

(name protohacks)

(generate_opam_files true)

(source
 (github cdaringe/protohacks))

(authors "cdaringe")

(maintainers "cdaringe")

(license MIT)

(documentation https://github.com/cdaringe/protohacks)

(package
 (name protohacks)
 (allow_empty)
 (synopsis "protohackers in OCaml")
 (description
  "TCP protocol implementations using OCaml 5.x with effects + eio")
 (depends
  (alcotest :with-test)
  (dune
   (> 3.4))
  containers
  eio
  eio_main
  ocaml-lsp-server
  ocamlformat
  ppx_deriving
  ppx_deriving_yojson
  utop
  yojson)
 (tags
  (topics eio effects tcp protocol streams)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

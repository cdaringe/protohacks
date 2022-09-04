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
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  (alcotest :with-test)
  (dune
   (> 3.4))
  eio
  eio_main
  ocaml-lsp-server
  ocamlformat
  ppx_deriving
  ppx_deriving_yojson
  utop
  yojson)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

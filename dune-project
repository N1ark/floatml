(lang dune 3.0)
(name floats)
(version 0.1.0)

(generate_opam_files true)

(source (github N1ark/floatml))
(license MIT)
(authors "Opale Sjöstedt")
(maintainers "opale.sjostedt@gmail.com")

(package
 (name floats)
 (synopsis "OCaml bindings for f16, f32, f64, f128 float types")
 (description "A library providing arbitrary precision float types with C bindings")
 (depends
  (ocaml (>= 4.14.0))
  (dune (>= 3.0))
  zarith))

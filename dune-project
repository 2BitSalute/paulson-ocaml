(lang dune 2.7)

(name paulson-ocaml)

(source
 (github 2BitSalute/paulson-ocaml))

(license MIT)

(authors "L. C. Paulson and Tatiana Racheva")

(maintainers "Tatiana Racheva")

(package
 (name paulson-1)
 (synopsis "Paulson's /ML for the Working Programmer/")
 (description "Various bits and pieces that I felt like trying")
 (depends
  dune-configurator
  (lwt
   (>= 5.0.0))
  (notty
   (and
    (>= 0.2.2)
    :with-test))
  (bos
   (and
    (>= 0.2.0)
    :with-test))
  (bechamel-notty
   (and
    (>= 0.1.0)
    :with-test))
  (bechamel
   (and
    (>= 0.1.0)
    :with-test))
  (logs
   (>= 0.5.0))
  cmdliner
  (fmt
   (>= 0.8.4))
  bigstringaf
  (alcotest :with-test)))

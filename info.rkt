#lang info
(define collection "anther")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib" "toml"))
(define scribblings '(("scribblings/anther.scrbl" ())))
(define pkg-desc "(An)o(ther) Pollen Toolkit")
(define version "0.0")
(define pkg-authors '(TheJoyfulInquiries))
(define license '(Apache-2.0 OR MIT))

(define raco-bin-paths
  '(("anther" "main.rkt")))

#lang info

(define collection 'multi)

(define deps '("base" "scribble-lib" "braw"))

(define scribblings '(("scribblings/swears.scrbl" ())
                      ("scribblings/h19.scrbl" ())))

(define pkg-desc "Heathkit H19(A) terminal handling library")

(define version "0.1")

(define pkg-authors '(jam))

(define license
  '(Apache-2.0 OR MIT))

(defpackage :openblas.ffi
  (:use))

(defpackage :openblas
  (:use #:cl #:alexandria #:autowrap.minimal #:plus-c)
  (:export
   #:saxpy
   #:sgemm
   ))

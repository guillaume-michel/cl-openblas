(in-package :openblas)

(cffi:define-foreign-library openblas
  (t (:default "libopenblas")))

(cffi:use-foreign-library openblas)

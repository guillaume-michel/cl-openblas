(in-package :openblas)

;;;; BLAS LEVEL 1

(defun sdsdot (n alpha x incx y incy)
  (cffi:with-pointer-to-vector-data (ptr-x x)
    (cffi:with-pointer-to-vector-data (ptr-y y)
      (openblas.ffi:cblas-sdsdot n alpha ptr-x incx ptr-y incy))))


(defun sdot (n x incx y incy)
  (cffi:with-pointer-to-vector-data (ptr-x x)
    (cffi:with-pointer-to-vector-data (ptr-y y)
    (openblas.ffi:cblas-sdot n ptr-x incx ptr-y incy))))

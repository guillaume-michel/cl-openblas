(in-package :openblas)

;;;; BLAS LEVEL 1

(defun sdsdot (n alpha x incx y incy)
  (cffi:with-pointer-to-vector-data (ptr-x x)
    (cffi:with-pointer-to-vector-data (ptr-y y)
      (openblas.ffi:cblas-sdsdot n alpha ptr-x incx ptr-y incy))))

(defun dsdot (n x incx y incy)
  (cffi:with-pointer-to-vector-data (ptr-x x)
    (cffi:with-pointer-to-vector-data (ptr-y y)
      (openblas.ffi:cblas-dsdot n ptr-x incx ptr-y incy))))

(macrolet ((def (lname cname)
             `(defun ,lname (n x incx y incy)
                (cffi:with-pointer-to-vector-data (ptr-x x)
                  (cffi:with-pointer-to-vector-data (ptr-y y)
                    (,cname n ptr-x incx ptr-y incy))))))
  (def sdot openblas.ffi:cblas-sdot)
  (def ddot openblas.ffi:cblas-ddot))

;; TODO Add missing functions with complex return type

(macrolet ((def (lname cname)
             `(defun ,lname (n x incx y incy)
                (c-with ((return-value openblas.ffi:openblas-complex-float))
                  (cffi:with-pointer-to-vector-data (ptr-x x)
                    (cffi:with-pointer-to-vector-data (ptr-y y)
                      (,cname return-value n ptr-x incx ptr-y incy)))
                  (complex (openblas.ffi:openblas-complex-float.real return-value)
                           (openblas.ffi:openblas-complex-float.imag return-value))))))
  (def cdotu openblas.ffi:cblas-cdotu)
  (def cdotc openblas.ffi:cblas-cdotc))

(macrolet ((def (lname cname)
             `(defun ,lname (n x incx)
                (cffi:with-pointer-to-vector-data (ptr-x x)
                    (,cname n ptr-x incx)))))
  (def sasum openblas.ffi:cblas-sasum)
  (def dasum openblas.ffi:cblas-dasum)
  (def scasum openblas.ffi:cblas-scasum)
  (def dzasum openblas.ffi:cblas-dzasum)

  (def snrm2 openblas.ffi:cblas-snrm2)
  (def dnrm2 openblas.ffi:cblas-dnrm2)
  (def scnrm2 openblas.ffi:cblas-scnrm2)
  (def dznrm2 openblas.ffi:cblas-dznrm2)

  (def isamax openblas.ffi:cblas-isamax)
  (def idamax openblas.ffi:cblas-idamax)
  (def icamax openblas.ffi:cblas-icamax)
  (def izamax openblas.ffi:cblas-izamax))

(macrolet ((def (lname cname)
             `(defun ,lname (n alpha x incx y incy)
                (cffi:with-pointer-to-vector-data (ptr-x x)
                  (cffi:with-pointer-to-vector-data (ptr-y y)
                    (,cname n alpha ptr-x incx ptr-y incy))))))
  (def saxpy openblas.ffi:cblas-saxpy)
  (def daxpy openblas.ffi:cblas-daxpy))

(defun caxpy (n alpha x incx y incy)
  (cffi:with-foreign-object (ptr-alpha :float 2)
    (setf (cffi:mem-aref ptr-alpha :float 0) (realpart alpha)
          (cffi:mem-aref ptr-alpha :float 1) (imagpart alpha))
    (cffi:with-pointer-to-vector-data (ptr-x x)
      (cffi:with-pointer-to-vector-data (ptr-y y)
        (openblas.ffi:cblas-caxpy n ptr-alpha ptr-x incx ptr-y incy)))))

(in-package :openblas)

;;;; BLAS LEVEL 3

(defun sgemm (order trans-a trans-b m n k alpha a lda b ldb beta c ldc)
  (cffi:with-pointer-to-vector-data (ptr-a a)
    (cffi:with-pointer-to-vector-data (ptr-b b)
      (cffi:with-pointer-to-vector-data (ptr-c c)
        (openblas.ffi:cblas-sgemm order trans-a trans-b
                                  m n k
                                  alpha
                                  ptr-a lda
                                  ptr-b ldb
                                  beta
                                  ptr-c ldc)))))


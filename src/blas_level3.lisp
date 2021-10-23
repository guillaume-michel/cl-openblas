(in-package :openblas)

;;;; BLAS LEVEL 3
;;;;

;; General Matrix-matrix multiply

;;;  single
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

;;;  double
(defun dgemm (order trans-a trans-b m n k alpha a lda b ldb beta c ldc)
  (cffi:with-pointer-to-vector-data (ptr-a a)
    (cffi:with-pointer-to-vector-data (ptr-b b)
      (cffi:with-pointer-to-vector-data (ptr-c c)
        (openblas.ffi:cblas-dgemm order trans-a trans-b
                                  m n k
                                  alpha
                                  ptr-a lda
                                  ptr-b ldb
                                  beta
                                  ptr-c ldc)))))
                                  
;;; complex
(defun cgemm (order trans-a trans-b m n k alpha a lda b ldb beta c ldc)
  (cffi:with-pointer-to-vector-data (ptr-a a)
    (cffi:with-pointer-to-vector-data (ptr-b b)
      (cffi:with-pointer-to-vector-data (ptr-c c)
        (openblas.ffi:cblas-cgemm order trans-a trans-b
                                  m n k
                                  alpha
                                  ptr-a lda
                                  ptr-b ldb
                                  beta
                                  ptr-c ldc)))))
                                  
;;; double-complex
(defun zgemm (order trans-a trans-b m n k alpha a lda b ldb beta c ldc)
  (cffi:with-pointer-to-vector-data (ptr-a a)
    (cffi:with-pointer-to-vector-data (ptr-b b)
      (cffi:with-pointer-to-vector-data (ptr-c c)
        (openblas.ffi:cblas-zgemm order trans-a trans-b
                                  m n k
                                  alpha
                                  ptr-a lda
                                  ptr-b ldb
                                  beta
                                  ptr-c ldc)))))
                                  
;; Symmetric matrix-matrix multiply
;;; 
;;; single
(defun ssymm (order side uplo m n alpha a lda b ldb beta c ldc)
  (cffi:with-pointer-to-vector-data (ptr-a a)
    (cffi:with-pointer-to-vector-data (ptr-b b)
      (cffi:with-pointer-to-vector-data (ptr-c c)
        (openblas.ffi:cblas-ssymm order side uplo
                                  m n
                                  alpha
                                  ptr-a lda
                                  ptr-b ldb
                                  beta
                                  ptr-c ldc)))))

;;;  double
(defun dsymm (order side uplo m n alpha a lda b ldb beta c ldc)
  (cffi:with-pointer-to-vector-data (ptr-a a)
    (cffi:with-pointer-to-vector-data (ptr-b b)
      (cffi:with-pointer-to-vector-data (ptr-c c)
        (openblas.ffi:cblas-dsymm order side uplo
                                  m n
                                  alpha
                                  ptr-a lda
                                  ptr-b ldb
                                  beta
                                  ptr-c ldc)))))

;;;  complex
(defun csymm (order side uplo m n alpha a lda b ldb beta c ldc)
  (cffi:with-pointer-to-vector-data (ptr-a a)
    (cffi:with-pointer-to-vector-data (ptr-b b)
      (cffi:with-pointer-to-vector-data (ptr-c c)
        (openblas.ffi:cblas-csymm order side uplo
                                  m n
                                  alpha
                                  ptr-a lda
                                  ptr-b ldb
                                  beta
                                  ptr-c ldc)))))

;;;  double-complex
(defun zsymm (order side uplo m n alpha a lda b ldb beta c ldc)
  (cffi:with-pointer-to-vector-data (ptr-a a)
    (cffi:with-pointer-to-vector-data (ptr-b b)
      (cffi:with-pointer-to-vector-data (ptr-c c)
        (openblas.ffi:cblas-zsymm order side uplo
                                  m n
                                  alpha
                                  ptr-a lda
                                  ptr-b ldb
                                  beta
                                  ptr-c ldc)))))

(in-package :openblas)

;;;; BLAS LEVEL 3
;;;;

;; General Matrix-matrix multiply
;; C = alpha*AB + beta*C
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
;; C = alpha* AB + beta*C
;; 
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

;; Symmetric rank k operations:
;; C = alpha * A A^T + beta * C
;; or
;; C = alpha * A^T A + beta * C

;;;  single
(defun ssyrk (order uplo trans n k alpha a lda beta c ldc)
  (cffi:with-pointer-to-vector-data (ptr-a a)
    (cffi:with-pointer-to-vector-data (ptr-c c)
      (openblas.ffi:cblas-ssyrk order uplo trans
                                n k
                                alpha
                                ptr-a
                                lda
                                beta
                                ptr-c
                                ldc))))


;;;  double
(defun dsyrk (order uplo trans n k alpha a lda beta c ldc)
  (cffi:with-pointer-to-vector-data (ptr-a a)
    (cffi:with-pointer-to-vector-data (ptr-c c)
      (openblas.ffi:cblas-dsyrk order uplo trans
                                n k
                                alpha
                                ptr-a
                                lda
                                beta
                                ptr-c
                                ldc))))


;;;  complex
(defun csyrk (order uplo trans n k alpha a lda beta c ldc)
  (cffi:with-pointer-to-vector-data (ptr-a a)
    (cffi:with-pointer-to-vector-data (ptr-c c)
      (openblas.ffi:cblas-csyrk order uplo trans
                                n k
                                alpha
                                ptr-a
                                lda
                                beta
                                ptr-c
                                ldc))))


;;;  double-complex
(defun zsyrk (order uplo trans n k alpha a lda beta c ldc)
  (cffi:with-pointer-to-vector-data (ptr-a a)
    (cffi:with-pointer-to-vector-data (ptr-c c)
      (openblas.ffi:cblas-zsyrk order uplo trans
                                n k
                                alpha
                                ptr-a
                                lda
                                beta
                                ptr-c
                                ldc))))


;;; Symmetric rank 2k operations
;;; C = alpha * A B^T + alpha * B A^T + beta*C
;;; or
;;; C = alpha * A^T B + alpha * B^T A + beta*C

;; single
(defun ssyr2k (order uplo trans n k alpha a lda b ldb beta c ldc)
  (cffi:with-pointer-to-vector-data (ptr-a a)
    (cffi:with-pointer-to-vector-data (ptr-b b)
      (cffi:with-pointer-to-vector-data (ptr-c c)
        (openblas.ffi:cblas-ssyr2k order uplo trans
                                   n k
                                   alpha
                                   ptr-a lda
                                   ptr-b ldb
                                   beta
                                   ptr-c ldc)))))

                                   
;; double
(defun dsyr2k (order uplo trans n k alpha a lda b ldb beta c ldc)
  (cffi:with-pointer-to-vector-data (ptr-a a)
    (cffi:with-pointer-to-vector-data (ptr-b b)
      (cffi:with-pointer-to-vector-data (ptr-c c)
        (openblas.ffi:cblas-dsyr2k order uplo trans
                                   n k
                                   alpha
                                   ptr-a lda
                                   ptr-b ldb
                                   beta
                                   ptr-c ldc)))))

                                   
;; complex
(defun csyr2k (order uplo trans n k alpha a lda b ldb beta c ldc)
  (cffi:with-pointer-to-vector-data (ptr-a a)
    (cffi:with-pointer-to-vector-data (ptr-b b)
      (cffi:with-pointer-to-vector-data (ptr-c c)
        (openblas.ffi:cblas-csyr2k order uplo trans
                                   n k
                                   alpha
                                   ptr-a lda
                                   ptr-b ldb
                                   beta
                                   ptr-c ldc)))))

                                   
;; double-complex
(defun zsyr2k (order uplo trans n k alpha a lda b ldb beta c ldc)
  (cffi:with-pointer-to-vector-data (ptr-a a)
    (cffi:with-pointer-to-vector-data (ptr-b b)
      (cffi:with-pointer-to-vector-data (ptr-c c)
        (openblas.ffi:cblas-zsyr2k order uplo trans
                                   n k
                                   alpha
                                   ptr-a lda
                                   ptr-b ldb
                                   beta
                                   ptr-c ldc)))))

                                   























(in-package :openblas)

(defun lisp-saxpy (z a x y)
  "Compute BLAS Level 1 SAXPY operation: zi = a * xi + yi with map-into CL procedure"
  (declare (type single-float a))
  (declare (type (simple-array single-float (*))
                 z x y))
  (declare (optimize (speed 3)
                     (compilation-speed 0)
                     (safety 0)
                     (debug 0)))
  (let ((f (lambda (xi yi)
             (+ (* a xi) yi))))
    (map-into z f x y)))

(defmacro timings (&body forms)
  (let ((real1 (gensym))
        (real2 (gensym))
        (run1 (gensym))
        (run2 (gensym))
        (result (gensym)))
    `(let* ((,real1 (get-internal-real-time))
            (,run1 (get-internal-run-time))
            (,result (progn ,@forms))
            (,run2 (get-internal-run-time))
            (,real2 (get-internal-real-time)))
       (values ,result
               (float (/ (- ,real2 ,real1) internal-time-units-per-second))
               (float (/ (- ,run2 ,run1) internal-time-units-per-second))))))

(defun gflops (m n k timing repeat)
  (/ (* 2 m n k repeat) (* timing 1000000000)))

(defun test (m n k &key (repeat 100) (file nil))
  (let ((order openblas.ffi:+CBLAS-ROW-MAJOR+)
        (trans-a openblas.ffi:+CBLAS-NO-TRANS+)
        (trans-b openblas.ffi:+CBLAS-NO-TRANS+)
        (alpha 1.f0)
        (beta 0.f0)
        (a (make-array (* m k) :element-type 'single-float :initial-element 0.f0))
        (b (make-array (* k n) :element-type 'single-float :initial-element 0.f0))
        (c (make-array (* m n) :element-type 'single-float :initial-element 0.f0))
        (lda k)
        (ldb n)
        (ldc n))
    (multiple-value-bind (res real run)
        (timings
          (dotimes (i repeat)
            (sgemm order trans-a trans-b m n k alpha a lda b ldb beta c ldc)))
      (declare (ignore res run))
      (format file "~A,~A,~A,~A~%" m n k (gflops m n k real repeat)))))

(defun test-saxpy (n &key (repeat 100) (file nil))
  (let ((alpha 1.f0)
        (x (make-array n :element-type 'single-float :initial-element 0.f0))
        (y (make-array n :element-type 'single-float :initial-element 0.f0))
        (incx 1)
        (incy 1))
    (multiple-value-bind (res real run)
        (timings
          (dotimes (i repeat)
            (saxpy n alpha x incx y incy)))
      (declare (ignore res run))
      (format file "~A,~A~%" n (/ (* 2 n repeat) (* real 1000000000))))))

(defun test-lisp-saxpy (n &key (repeat 100) (file nil))
  (let ((alpha 1.f0)
        (x (make-array n :element-type 'single-float :initial-element 0.f0))
        (y (make-array n :element-type 'single-float :initial-element 0.f0))
        (z (make-array n :element-type 'single-float :initial-element 0.f0)))
    (multiple-value-bind (res real run)
        (timings
          (dotimes (i repeat)
            (lisp-saxpy z alpha x y)))
      (declare (ignore res run))
      (format file "~A,~A~%" n (/ (* 2 n repeat) (* real 1000000000))))))


(defun full-bench (filename num-threads)
  (openblas::set-num-threads num-threads)
  (with-open-file (file (concatenate 'string filename "_" (write-to-string num-threads) ".csv")
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (loop :for m :from 500 :to 1000 :by 100 :do
         (loop :for n :from 500 :to 1000 :by 100 :do
              (loop :for k :from 500 :to 1000 :by 100 :do
                   (test m n k :repeat 10 :file file))))))

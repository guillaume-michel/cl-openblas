(in-package :openblas)

(defmacro timing (&body forms)
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
        (timing
          (dotimes (i repeat)
            (sgemm order trans-a trans-b m n k alpha a lda b ldb beta c ldc)))
      (declare (ignore res run))
      (format file "~A,~A,~A,~A~%" m n k (gflops m n k real repeat)))))


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

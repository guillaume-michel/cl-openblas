(in-package :openblas)

(defun get-num-procs ()
  (openblas.ffi:openblas-get-num-procs))

(defun get-num-threads ()
  (openblas.ffi:openblas-get-num-threads))

(defun set-num-threads (num-threads)
  (openblas.ffi:openblas-set-num-threads num-threads))

(defsystem "cl-openblas"
  :description "OpenBLAS wrapper for Common Lisp"
  :version "0.0.1"
  :author "Guillaume MICHEL <contact@orilla.fr>"
  :license "MIT license (see COPYING)"
  :depends-on ("alexandria"
               "cl-autowrap/libffi"
               "cffi")
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "autowrap")
               (:file "library")
               (:file "blas_level1")
               (:file "blas_level2")
               (:file "blas_level3")
               (:file "openblas")
               (:file "bench")
               (:module #:specs
                        :pathname "specs"
                        :components
                        ((:static-file "openblas.h")))))

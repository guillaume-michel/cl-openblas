(in-package :openblas.ffi)

(autowrap:c-include '(cl-openblas specs "openblas.h")
                    :spec-path '(cl-openblas specs)
                    :sysincludes `,(cl:append
                                    #+(and unix (not darwin))
                                    (cl:list "/usr/include/x86_64-linux-gnu/"))
                    :exclude-definitions (".*")
                    :include-definitions ("^cblas_"
                                          "^blasint$"
                                          "^CBLAS_ORDER$"
                                          "^CBLAS_TRANSPOSE$"
                                          "^CBLAS_UPLO$"
                                          "^CBLAS_SIDE$"
                                          "^CBLAS_DIAG$"
                                          "^size_t$"
                                          "^openblas_complex_float$"
                                          "^openblas_complex_double$"
                                          "^openblas_set_num_threads$"
                                          "^openblas_get_num_threads$"
                                          "^openblas_get_num_procs$"))

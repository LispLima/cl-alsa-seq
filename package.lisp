;;;; package.lisp

(defpackage #:cl-alsaseq
  (:use #:cl #:cffi #:calispel #:optima #:optima.extra)
  (:import-from :let-over-lambda
                #:g!-symbol-p
                #:defmacro/g!
                #:o!-symbol-p
                #:o!-symbol-to-g!-symbol
                #:defmacro!))

(defpackage #:cl-alsaseq.ez
  (:use #:cl #:cl-alsaseq))

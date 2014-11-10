(in-package :cl-alsaseq)
(defun generate-binding* (name headers &rest args
                          &key (working-directory (verrazano::system-relative-pathname
                                                   :verrazano "example-bindings/"))
                          (debug nil)
                          (gccxml-flags "-I/usr/include")
                          &allow-other-keys)
  (format *debug-io* "~%~%; *** Processing binding ~S~%" name)
  (remove-from-plistf args :working-directory :gccxml-flags :debug)
  (block try
    (handler-bind ((serious-condition
                    (lambda (error)
                      (unless debug
                        (warn "Failed to generated binding for ~S, error: ~A" name error)
                        (return-from try)))))
      (let ((*print-right-margin* 100))
        (generate-binding (append
                           (list :cffi
                                 :package-name name
                                 :input-files headers
                                 :working-directory working-directory
                                 :gccxml-flags gccxml-flags)
                           args)
                          :keep-temporary-files nil))))
  (values))

(defun generate-alsa-binding ()
  (generate-binding* :cl-alsaseq.binding '("stdio.h" "sys/poll.h" "alsa/input.h" "alsa/output.h" "alsa/conf.h" "alsa/seq_event.h")
                     :working-directory (verrazano::system-relative-pathname :cl-alsaseq "bindings/")))

(defun ev-start ()
  (interactive)
  (slime-interactive-eval "(print (cl-alsaseq.util::ev-start))"))

(global-set-key (kbd "<f8>") 'ev-start)

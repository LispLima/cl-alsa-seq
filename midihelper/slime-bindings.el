(defun ev-start ()
  (interactive)
  (slime-interactive-eval "(print (ev-start))"))

(global-set-key (kbd "<f8>") 'ev-start)

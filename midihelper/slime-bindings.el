(defun test-midi-ev ()
  (interactive)
  (slime-interactive-eval "(print (cl-alsaseq.util::ev-start))"))

(global-set-key (kbd "<f8>") 'test-midi-ev)

(defun ev-start ()
  (interactive)
  (slime-interactive-eval "(calispel:! cl-alsaseq.util::*slave-tick-chan* cl-alsaseq.util::(ev-start))"))

(defun ev-stop ()
  (interactive)
  (slime-interactive-eval "(calispel:! cl-alsaseq.util::*slave-tick-chan* cl-alsaseq.util::(ev-stop))"))

(defun ev-continue ()
  (interactive)
  (slime-interactive-eval "(calispel:! cl-alsaseq.util::*slave-tick-chan* cl-alsaseq.util::(ev-continue))"))

(global-set-key (kbd "M-m") 'ev-stop)
(global-set-key (kbd "M-k") 'ev-continue)
(global-set-key (kbd "M-l") 'ev-start)

(defun test-midi-ev ()
  (interactive)
  (slime-interactive-eval "(print (midihelper:ev-start))"))

(global-set-key (kbd "<f8>") 'test-midi-ev)

(defun ev-start ()
  (interactive)
  (slime-interactive-eval "(calispel:! midihelper:*ctrl-chan* midihelper:(ev-start))"))

(defun ev-stop ()
  (interactive)
  (slime-interactive-eval "(calispel:! midihelper:*ctrl-chan* midihelper:(ev-stop))"))

(defun ev-continue ()
  (interactive)
  (slime-interactive-eval "(calispel:! midihelper:*ctrl-chan* midihelper:(ev-continue))"))

(global-set-key (kbd "M-m") 'ev-stop)
(global-set-key (kbd "M-k") 'ev-continue)
(global-set-key (kbd "M-l") 'ev-start)

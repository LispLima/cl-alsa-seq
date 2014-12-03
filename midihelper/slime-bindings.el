(defun test-midi-ev ()
  (interactive)
  (slime-interactive-eval "(print (midihelper:ev-start))"))

(global-set-key (kbd "<f8>") 'test-midi-ev)

(defun ev-start ()
  (interactive)
  (slime-interactive-eval "(calispel:! midihelper:*clock-ctrl-chan* (midihelper:ev-start))"))

(defun ev-stop ()
  (interactive)
  (slime-interactive-eval "(calispel:! midihelper:*clock-ctrl-chan* (midihelper:ev-stop))"))

(defun ev-continue ()
  (interactive)
  (slime-interactive-eval "(calispel:! midihelper:*clock-ctrl-chan* (midihelper:ev-continue))"))

(defun active0 ()
  (interactive)
  (slime-interactive-eval "(mloops::active-loop 0)"))
(defun active1 ()
  (interactive)
  (slime-interactive-eval "(mloops::active-loop 1)"))
(defun active2 ()
  (interactive)
  (slime-interactive-eval "(mloops::active-loop 2)"))
(defun active3 ()
  (interactive)
  (slime-interactive-eval "(mloops::active-loop 3)"))
(defun activen ()
  (interactive)
  (slime-interactive-eval "(mloops::active-loop 3)"))

(global-unset-key (kbd "M-m"))

(global-set-key (kbd "M-m M-m") 'ev-stop)

(global-set-key (kbd "M-m M-k") 'ev-continue)

(global-set-key (kbd "M-m M-l") 'ev-start)

(defun notekick ()
  (interactive)
  (slime-interactive-eval "(calispel:! midihelper:*reader-ochan* (midihelper:ev-noteon 0 36 127))"))
(defun notesnare ()
  (interactive)
  (slime-interactive-eval "(calispel:! midihelper:*reader-ochan* (midihelper:ev-noteon 0 38 127))"))
(defun noteclap ()
  (interactive)
  (slime-interactive-eval "(calispel:! midihelper:*reader-ochan* (midihelper:ev-noteon 0 39 127))"))
(defun note3 ()
  (interactive)
  (slime-interactive-eval "(calispel:! midihelper:*reader-ochan* (midihelper:ev-noteon 0 43 127))"))

(global-set-key (kbd "M-n") 'note3)

(global-set-key (kbd "M-j") 'notekick)

(global-set-key (kbd "M-k") 'notesnare)

(global-set-key (kbd "M-l") 'noteclap)

(defun loop1 ()
  (interactive)
  (slime-interactive-eval "(mloops::active-loop 1)"))
(defun loop2 ()
  (interactive)
  (slime-interactive-eval "(mloops::active-loop 2)"))
(defun loop3 ()
  (interactive)
  (slime-interactive-eval "(mloops::active-loop 3)"))
(defun loop4 ()
  (interactive)
  (slime-interactive-eval "(mloops::active-loop 4)"))
(defun loopmetro ()
  (interactive)
  (slime-interactive-eval "(mloops::active-loop :metro)"))

(global-set-key (kbd "M-m 1") 'loop1)
(global-set-key (kbd "M-m 2") 'loop2)
(global-set-key (kbd "M-m 3") 'loop3)
(global-set-key (kbd "M-m 4") 'loop4)
(global-set-key (kbd "M-m 0") 'loopmetro)

(defun midi-loop-cycle ()
  (interactive)
  (slime-interactive-eval "(calispel:! midihelper:*reader-ochan* (ev-loop-cycle))"))
(global-set-key (kbd "M-SPC") 'midi-loop-cycle)


(defun loop-play ()
  (interactive)
  (slime-interactive-eval "(calispel:! midihelper:*reader-ochan* (midiloops::ev-loop-play))"))
(defun loop-play-push-extend ()
  (interactive)
  (slime-interactive-eval "(calispel:! midihelper:*reader-ochan* (midiloops::ev-loop-push-extend))"))
(defun loop-stop ()
  (interactive)
  (slime-interactive-eval "(calispel:! midihelper:*reader-ochan* (midiloops::ev-loop-stop))"))
(defun loop-erase ()
  (interactive)
  (slime-interactive-eval "(calispel:! midihelper:*reader-ochan* (midiloops::ev-loop-erase))"))
(defun loop-overwrite ()
  (interactive)
  (slime-interactive-eval "(calispel:! midihelper:*reader-ochan* (midiloops::ev-loop-overwrite))"))
(defun loop-overdub ()
  (interactive)
  (slime-interactive-eval "(calispel:! midihelper:*reader-ochan* (midiloops::ev-loop-overdub))"))

(global-set-key (kbd "M-m m") 'loop-stop)
(global-set-key (kbd "M-m k") 'loop-continue)
(global-set-key (kbd "M-m l") 'loop-play)
(global-set-key (kbd "M-m d") 'loop-erase)
(global-set-key (kbd "M-m O") 'loop-overwrite)
(global-set-key (kbd "M-m o") 'loop-overdub)
(global-set-key (kbd "M-m l") 'loop-play-push-extend)
(global-set-key (kbd "M-m k") 'loop-play)


;; (define-derived-mode
;; (ido
;; (smex
;; smart parens
;; cider mode
;; redshank refactoring library

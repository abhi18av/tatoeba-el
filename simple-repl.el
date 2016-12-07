(require 'dash)
(require 'cl-lib)
(require 's)


(defun make-simple-repl-variables ()

  (set (make-local-variable 'simple-repl-sent) nil)
  )




(defvar simple-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\n" 'simple-repl-read-print)
    (define-key map "\r" 'simple-repl-ret-or-read)
    map))


(define-derived-mode simple-repl-mode text-mode "simple-repl"
  (make-simple-repl-variables)
  (insert "In the beginning was the word ...")
 ; (insert (propertize "\n=>> " 'face '(:foreground "green" )))
  (insert "\n=>> ")
  )




(add-hook 'simple-repl-mode-hook (lambda ()
                                   (font-lock-mode -1)) 'append)


;;;###autoload
(defun simple-repl ()
  (interactive)
  (switch-to-buffer "*simple-repl*")
  (simple-repl-mode))









(defun simple-repl-ret-or-read (arg)
  (interactive "*p")
  (if (= (preceding-char) ?\n)
      (simple-repl-read-print)
    (newline arg)))






(defun simple-repl-read-print ()
  "Top level loop."
  (interactive)

  (setq simple-repl-sent (simple-repl-readin))
  (insert "\n# ")

  (simple-repl-response)

 ; (insert (propertize "\n=>> " 'face '(:foreground "green" )))

  (insert "\n=>> ")
  )





(defvar sentence nil)
(defun simple-repl-readin ()
  (progn
    (previous-line 1)
    (setq sentence
          (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

    (next-line 1)
    )

  sentence
  )





(defun simple-repl-response ()
  (if
      (-contains-p (s-split " " sentence) "are")

    (insert (propertize "rrr" 'face '(:foreground "red")))

    (insert (propertize "hmm" 'face '(:foreground "skyblue"))))

    (insert "\n")
  )




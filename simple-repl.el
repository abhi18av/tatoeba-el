(require 'dash)
(require 'cl-lib)
(require 's)

(defvar simple-repl--lincount)

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
  (insert "\n@ "))




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

  (simple-repl-doc)

  (insert "\n@ ")

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





(defun simple-repl-doc ()
  (if
      (-contains-p (s-split " " sentence) "are")
      (insert "Rrrrr")
    (insert "hmm"))

    (insert "\n")
  )




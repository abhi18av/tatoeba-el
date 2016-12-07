
(with-current-buffer "*simple-repl*"
(defun simple-repl-readin ()
  "Read a sentence.  Return it as a list of words."
  (let (sentence)
    (setq sentence (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

    sentence)
  )
(s-split-words (simple-repl-readin)))




(with-current-buffer "*simple-repl*"

  (defvar sentence nil)
  (defvar sentence-word-list nil)

  (setq sentence (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

  (setq sentence-word-list
        (-map (lambda (x) ( make-symbol x)) (s-split-words sentence))))





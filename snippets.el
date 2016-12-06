(with-current-buffer "x"
(defun simple-repl-readin ()
  "Read a sentence.  Return it as a list of words."
  (let (sentence)
    (setq sentence (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

    sentence))

(simple-repl-readin))

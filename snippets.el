
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




(progn

  (setq a-ls '(("a". 1) ("b". 2) ("c". 3)))

  (setq p-ls '("a" 1 "b" 2 "c" 3))
)


;; this one uses equal instead of eq
(plist-get '(foo 4) 'foo)




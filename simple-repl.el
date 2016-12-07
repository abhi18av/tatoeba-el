

(require 'dash)
(require 'cl-lib)
(require 's)

(defvar simple-repl--lincount)

(defun make-simple-repl-variables ()

  (set (make-local-variable 'simple-repl-sent) nil)
  (set (make-local-variable 'simple-repl--lincount) 0))




(defvar simple-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\n" 'simple-repl-read-print)
    (define-key map "\r" 'simple-repl-ret-or-read)
    map))

(define-derived-mode simple-repl-mode text-mode "simple-repl"
 (make-simple-repl-variables)
  (insert "In the beginning was the word ...")
  (insert "\n"))

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
  (insert "\n ")
  (setq simple-repl--lincount (1+ simple-repl--lincount))
  (simple-repl-doc)
  (insert "\n ")

  )


(defvar sentence nil)
(defvar sentence-word-list nil)



(defun simple-repl-readin ()
  (setq sentence (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

  (setq sentence-word-list
        (s-split " " sentence))

  sentence-word-list
  )



(defun simple-repl-doc ()
  (cond

   ((-contains-p simple-repl-sent "are")
    (insert  "You said Rrrrr\n"))

   (t 
      (insert "default\n" ))))








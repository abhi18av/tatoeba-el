

(require 'dash)
(require 'cl-lib)
(require 's)

(defvar simple-repl--lincount)

(defun make-simple-repl-variables ()

  (set (make-local-variable 'simple-repl-sent) nil)
  (set (make-local-variable 'simple-repl--lincount) 0))



(defmacro simple-repl$ (what)
  "Quoted arg form of simple-repl-$."
  `(simple-repl-$ ',what))

(defun simple-repl-$ (what)
  "Return the car of a list, rotating the list e=======
ach time."
  (let* ((vv (symbol-value what))
         (first (car vv))
         (ww (append (cdr vv) (list first))))
    (set what ww)
    first))

(defvar simple-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\n" 'simple-repl-read-print)
    (define-key map "\r" 'simple-repl-ret-or-read)
    map))

(define-derived-mode simple-repl-mode text-mode "simple-repl"
  "Major mode for running the Doctor (Eliza) program.
Like Text mode with Auto Fill mode
except that RET when point is after a newline, or LFD at any time,
reads the sentence before point, and prints the Doctor's answer."
  (make-simple-repl-variables)
 ; (setq font-lock-mode nil)
  (insert "In the beginning was the word ...")
  (insert "\n"))

(add-hook 'simple-repl-mode-hook (lambda ()
                             (font-lock-mode -1)) 'append)


;; Define equivalence classes of words that get treated alike.

;;;###autoload
(defun simple-repl ()
  "Switch to *doctor* buffer and start giving psychotherapy."
  (interactive)
  (switch-to-buffer "*simple-repl*")
  (simple-repl-mode))

(defun simple-repl-ret-or-read (arg)
  "Insert a newline if preceding character is not a newline.
Otherwise call the Doctor to parse preceding sentence."
  (interactive "*p")
  (if (= (preceding-char) ?\n)
      (simple-repl-read-print)
    (newline arg)))

(defun simple-repl-read-print ()
  "Top level loop."
  (interactive)
;; change the input method
;;  (set-input-method "latin-alt-postfix")

;;  (toggle-input-method)



  (setq simple-repl-sent (simple-repl-readin))
  (insert "\n@ ")
  (setq simple-repl--lincount (1+ simple-repl--lincount))
  (simple-repl-doc)
  (insert "\n=> ")

;;  ;; Reset the method
;;  (set-input-method "utf-8")
;;  (toggle-input-method)

  )

;; (defun simple-repl-readin ()
;;   "Read a sentence.  Return it as a list of words."
;;   (let (sentence)
;;     (backward-sentence 1)
;;     (while (not (eobp))
;;       (setq sentence (append sentence (list (simple-repl-read-token)))))
;;     sentence))

;; (defun simple-repl-read-token ()
;;   "Read one word from buffer."
;;   (prog1 (intern (downcase (buffer-substring (point)
;; 					     (progn
;; 					       (forward-word 1)
;; 					       (point)))))
;;     (re-search-forward "\\Sw*")))
;; Main processing function for sentences that have been read.



(defvar sentence nil)
(defvar sentence-word-list nil)



(defun simple-repl-readin ()
  (setq sentence (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

;  sentence



; (setq sentence-word-list
;      (-map (lambda (x) ( make-symbol x)) (s-split-words sentence)))

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








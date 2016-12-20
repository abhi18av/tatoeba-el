(require 'dash)
(require 'cl-lib)
(require 's)


(defun make-simple-repl-variables ()

  (set (make-local-variable 'simple-repl-sent) nil)
  )

(add-hook 'simple-repl-mode-hook (lambda ()
                                   (font-lock-mode -1)) 'append)





(defvar simple-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\n" 'simple-repl-read-print)
    (define-key map "\r" 'simple-repl-ret-or-read)
    map))


(define-derived-mode simple-repl-mode text-mode "simple-repl"
  (make-simple-repl-variables)
  (insert "In the beginning was the word ...")
 ;  (inse(lax-plist-get p-ls "a")rt "\n=>> ")

  (progn
    (insert (propertize "\n=>>" 'face '(:foreground "green" )))
    (insert " "))


  )





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

;  (insert "\n# ")
  (progn 
    (insert (propertize "\n#" 'face '(:foreground "purple" )))
    (insert " "))


  (simple-repl-response)
  (progn 
  (insert (propertize "\n=>>" 'face '(:foreground "green" )))
  (insert " "))

  ;(insert "\n=>> ")
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


;; using a property list for this purpose
(defvar response-list nil)
;; using SETQ instead of DEFVAR
(setq response-list  '("one" 1 "two" 2 "three" 3))




(defun simple-repl-response ()
  (cond

   ;; if the sentence contains only ONE word and the word is in the response list

   ((equal
     (length (s-split " " sentence))          2)
;    (insert (second (s-split " " sentence))))
     (insert (number-to-string (lax-plist-get response-list
                            (second (s-split " " sentence))))))


   ((-contains-p (s-split " " sentence) "are")
    (insert (propertize "rrr" 'face '(:foreground "red"))))

    ;; default case for cond
    (t (insert (propertize "hmm" 'face '(:foreground "skyblue")))))



;  (insert (number-to-string (length (s-split " " sentence))))

    (insert "\n")
  )


;; (defvar response-list  '("one" 1 "two" 2 "three" 3))
;; response-list
;; ELISP> (lax-plist-get response-list "one")
;; nil

(require 'dash)
(require 'cl-lib)
(require 's)
(require 'ht)
(require 'json)

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
 ;  (insert "\n=>> ")

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





(defun simple-repl-response ()
;; replace this with a simple loop that compares the sentences/words and returns the correction
   (if
      (-contains-p (s-split " " sentence) "are")


    (insert (propertize "hmm" 'face '(:foreground "skyblue"))))

    (insert "\n")
  )



;;;;;;;;;;;;



(with-current-buffer "*scratch*"

(defvar s1 '("a" "b" "c"))

(defvar s2 '("a" "b" "c"))

(defvar s3 '(1 2 3))

(defvar s4 '("a" 2 3 ))

;; also need to check if the response is of the correct length

(defun correct-or-return-incorrect-indices (split-sentence1 split-sentence2)

;  (let (incorrect-word-index nil)

  (setq-default incorrect-word-index nil)

  (if
      (equalp split-sentence1 split-sentence2 )
      (print "correct")
    ;; the case to pinpoint which words are wrong
    (dotimes (i (length split-sentence1))
      (if
          (not
           (equalp
            (elt split-sentence1 i)
            (elt split-sentence2 i)))

          (setq incorrect-word-index (cons i incorrect-word-index)))))

(reverse incorrect-word-index)
;  (setq incorrect-word-index nil)
  )


(defun print-incorrect-indices ( list-of-indices split-sentence1 split-sentence2)

  (dolist (i list-of-indices)
    (insert-string "\n"
                   (propertize (elt split-sentence1 i) 'face '(:foreground "red"))
                   " => "
                   (propertize (elt split-sentence2 i) 'face '(:foreground "green")))))


(print-incorrect-indices (
                           (correct-or-return-incorrect-indices s1 s3)

                          s1 s3))


(defun convert-non-numeric-to-string (ls)
  (setq-default stringified nil)
  ( dolist (i ls)

    ;; convert this to consider cases like << 15:10 >>
    ;; use cond
    (if (stringp i)
        (setq stringified (cons stringified i))
      (setq stringified (cons stringified
            (-map (lambda (x) (number-to-string x))  (list i))))))
  stringified)



(convert-non-numeric-to-string '("a" "12 3 4" 14:64)))
)

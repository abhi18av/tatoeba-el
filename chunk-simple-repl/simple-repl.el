(add-to-load-path "~/Projects/Amsterdam/tatoeba-el/chunk-simple-repl/")
(require 'simple-repl-response)

; (add-to-list 'load-path "~/Projects/Amsterdam/tatoeba-el/modules-test/")




(require 'dash)
(require 'cl-lib)
(require 's)
(require 'ht)



(defun make-simple-repl-variables ()

  (set (make-local-variable 'simple-repl-sent) nil)
  )

(add-hook 'simple-repl-mode-hook (lambda ()
                                   (font-lock-mode -1)) 'append)


(add-hook 'simple-repl-mode-hook (lambda () 
                             (company-mode -1)) 'append)


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

    ;; here we add the function to handle the string  to be translated.

    ; @ 你好马？ 
    ;=>> :en hello / Hello
    ; # correct/ retry

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
(setq response-list  '(":1" "one" ":2" "two" ":3" "three"))


(defvar response-list-ht nil)
(setq response-list-ht (ht<-plist response-list))





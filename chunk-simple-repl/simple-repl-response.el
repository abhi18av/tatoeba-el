




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




(defun simple-repl-response ()
  (cond

   ;; if the sentence contains only ONE word and the word is in the response list

   (
    (ht-contains-p response-list-ht (second (s-split " " sentence)))
    (insert  (lax-plist-get response-list
                            (second (s-split " " sentence)))))


   ((-contains-p (s-split " " sentence) "are")
    (insert (propertize "rrr" 'face '(:foreground "red"))))

   ;; default case for cond
   (t (insert (propertize "hmm" 'face '(:foreground "skyblue")))))



                                        ;  (insert (number-to-string (length (s-split " " sentence))))

  (insert "\n")

  )

(provide 'simple-repl-response)

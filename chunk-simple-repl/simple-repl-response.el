


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

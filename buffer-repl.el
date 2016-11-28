(define-derived-mode tatoeba-mode text-mode "Tatoeba"
  "Major mode for running Tatoeba."
  :prefix "tatoeba-"
  :group 'games)




(defun tatoeba ()
  (switch-to-buffer "*tatoeba*")
  (tatoeba-mode))

(with-current-buffer "*tatoeba*"
  (message-box "Welcome to Tatoeba!\n"))





(provide 'tatoeba)
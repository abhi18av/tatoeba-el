(define-derived-mode tatoeba-mode text-mode "Tatoeba"
  "Major mode for running Tatoeba."
  :prefix "tatoeba-"
  :group 'games)




(defun tatoeba ()
  (interactive)
  (switch-to-buffer "*tatoeba*")
  (tatoeba-mode)
  (tatoeba-welcome))


(setq font-lock-mode nil)

(defun tatoeba-welcome ()
(with-current-buffer "*tatoeba*"
  (setf (buffer-string) " ")
  (insert "Welcome to Tatoeba!\n")
  (goto-char (point-max)))
)

(tatoeba)



(provide 'tatoeba)

(define-derived-mode tatoeba-mode text-mode "Tatoeba")


(defun tatoeba ()
  (interactive)
  (switch-to-buffer "*tatoeba*")
  (tatoeba-mode))

(setq-local font-lock-mode nil)

(mapc
 (lambda (x) (
              insert (propertize " color " 'face (list :foreground x))))
 '("red" "green" "orange"))

(provide 'tatoeba)

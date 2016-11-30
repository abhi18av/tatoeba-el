(defgroup tatoeba nil
  :prefix "tat-"
  :group 'games)

(define-derived-mode tatoeba-mode text-mode "Tatoeba")


(defun tatoeba ()
  (interactive)
  (switch-to-buffer "*tatoeba*"))



(mapc
 (lambda (x) (
              insert (propertize " color " 'face (list :foreground x))))
 '("red" "green" "orange"))
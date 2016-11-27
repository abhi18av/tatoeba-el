(defgroup tatoeba nil
  :prefix "tat-"
  :group games)

(define-derived-mode tatoeba-mode text-mode "Tatoeba"

  (make-local-variable 'scroll-step)
  (setq scroll-step 2))


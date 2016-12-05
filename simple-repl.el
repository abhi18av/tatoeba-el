(defmacro simple-repl$ (what)
  "Quoted arg form of simple-repl-$."
  `(simple-repl-$ ',what))

(defun simple-repl-$ (what)
  "Return the car of a list, rotating the list each time."
  (let* ((vv (symbol-value what))
         (first (car vv))
         (ww (append (cdr vv) (list first))))
    (set what ww)
    first))



(require 'cl)
(require 's)
(require 'dash)


(defgroup dunnet nil
  "Learning Languages in Emacs"
  :prefix "lang-repl"
  :group 'games)

(setq line-read nil)

(defun line-read-function ()
  (setq line-read
    (read-string "=> ")))


(line-read-function)


(setq chopped-string
      (s-split " " line-read))

(progn
  (switch-to-buffer "x")
  (setf (buffer-string) chopped-string))

                                        ;(message line-read)
;(message-box line-read)


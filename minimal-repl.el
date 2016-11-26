

(require 'cl)
(require 's)
(require 'dash)


(defgroup dunnet nil
  "Learning Languages in Emacs"
  :prefix "lang-repl"
  :group 'games)


(setq line-read
    (read-string "=> "))


(message line-read)

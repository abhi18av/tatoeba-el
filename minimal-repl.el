

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
  (setf (buffer-string) line-read))

                                        ;(message line-read)
;(message-box line-read)



(defun dun-parse (arg)
  "Function called when return is pressed in interactive mode to parse line."
  (interactive "*p")
  (beginning-of-line)
  (let
      ((beg (1+ (point)))
       line)

    (end-of-line)

    (if
        (and (not (= beg (point))) (not (< (point) beg))
             (string= ">" (buffer-substring (- beg 1) beg)))

        (progn
          (setq line (downcase (buffer-substring beg (point))))
          (princ line)
          (if (equal line-read "abcd")
              (princ "I don't understand that.\n")))

      (goto-char (point-max))
      (princ "\n"))))
  

(dun-parse (line-read))




(require 'cl)
(require 's)
(require 'dash)


(defgroup dunnet nil
  "Learning Languages in Emacs"
  :prefix "lang-repl"
  :group 'games)


;;;; Mode definitions for interactive mode

(define-derived-mode lang-mode text-mode "lang-repl"
  "Major mode for lang-repl.")





(defun dun-parse (arg)
  "Function called when return is pressed in interactive mode to parse line."
  (interactive "*p")
  (beginning-of-line)
  (let ((beg (1+ (point)))
        line)
    (end-of-line)
    (if (and (not (= beg (point))) (not (< (point) beg))
             (string= ">" (buffer-substring (- beg 1) beg)))
        (progn
          (setq line (buffer-substring beg (point)))
          (princ line)
          (if (equal "a" "a")
              (princ "You entered \"a\" \n")))
      (goto-char (point-max))
      (dun-mprinc "\n")))
  (dun-messages))

(defun dun-messages ()
  (if dun-dead
      (text-mode)
    (if (eq dungeon-mode 'dungeon)
        (progn
          (if (not (= room dun-current-room))
              (progn
                (dun-describe-room dun-current-room)
                (setq room dun-current-room)))
          (dun-fix-screen)
          (dun-mprinc ">")))))

(defun tatoeba ()
  "Switch to *tatoeba* buffer and start game."
  (interactive)
  (switch-to-buffer "*tatoeba*")
  (dun-mode)
  (setq dun-dead nil)
  (setq room 0)
  (dun-messages))



(defgroup dunnet nil
  "Learning Languages in Emacs"
  :prefix "lang-repl"
  :group 'games)


;;;; Mode definitions for interactive mode

(define-derived-mode dun-mode text-mode "lang-repl"
  "Major mode for lang-repl."
  (make-local-variable 'scroll-step)
  (setq scroll-step 2))

(defun dun-parse (arg)
  "Function called when return is pressed in interactive mode to parse line."
  (interactive "*p")
  (beginning-of-line)
  (let ((beg (1+ (point)))
        line)
    (end-of-line)
    (if (and (not (= beg (point))) (not (< (point) beg))
             (string= "=>> " (buffer-substring (- beg 1) beg)))
        (progn
          (setq line (downcase (buffer-substring beg (point))))
          (princ line)
          (if (eq (dun-vparse dun-ignore dun-verblist line) -1)
              (dun-mprinc "I don't understand that.\n")))
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
	  (dun-mprinc "=>> ")))))

;;;###autoload
(defun lang-repl ()
  "Switch to *lang-repl* buffer and start game."
  (interactive)
  (switch-to-buffer "*lang-repl*")
  (dun-mode)
  (setq dun-dead nil)
  (setq room 0)
  (dun-messages))

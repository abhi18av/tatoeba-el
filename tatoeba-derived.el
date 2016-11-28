
;;;; Mode definitions for interactive mode

(define-derived-mode dun-mode text-mode "Tatoeba"
  "Major mode for running Tatoeba."
  (make-local-variable 'scroll-step)
  (setq scroll-step 2))

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
	  (dun-mprinc ">")))))

;;;###autoload
(defun tatoeba ()
  "Switch to *tatoeba* buffer and start game."
  (interactive)
  (switch-to-buffer "*tatoeba*")
  (dun-mode)
  (setq dun-dead nil)
  (setq room 0)
  (dun-messages))

;;;;
;;;; This section contains all of the verbs and commands.
;;;;

;;; There is a special object in the room.  This object's description,
;;; or lack thereof, depends on certain conditions.



;;; Print every object in player's inventory.  Special case for the jar,
;;; as we must also print what is in it.
;;; Dropping certain things causes things to happen.
;;; Give long description of current room, or an object.
;;; We try to take an object that is untakable.  Print a message
;;; depending on what it is.
;;; Various movement directions

(defun dun-go (args)
  (if (or (not (car args))
	  (eq (dun-doverb dun-ignore dun-verblist (car args)
			  (cdr (cdr args))) -1))
      (dun-mprinc "I don't understand where you want me to go.\n")))

;;; Uses the dungeon-map to figure out where we are going.  If the
;;; requested direction yields 255, we know something special is
;;; supposed to happen, or perhaps you can't go that way unless
;;; certain conditions are met.
;;; Movement in this direction causes something special to happen if the
;;; right conditions exist.  It may be that you can't go this way unless
;;; you have a key, or a passage has been opened.

;;; coding note: Each check of the current room is on the same 'if' level,
;;; i.e. there aren't else's.  If two rooms next to each other have
;;; specials, and they are connected by specials, this could cause
;;; a problem.  Be careful when adding them to consider this, and
;;; perhaps use else's.

;;;;
;;;;  This section defines various utility functions used
;;;;  by dunnet.
;;;;


;;; Function which takes a verb and a list of other words.  Calls proper
;;; function associated with the verb, and passes along the other words.


;;; Function to take a string and change it into a list of lowercase words.

(defun dun-listify-string (strin)
  (let (pos ret-list end-pos)
    (setq pos 0)
    (setq ret-list nil)
    (while (setq end-pos (string-match "[ ,:;]" (substring strin pos)))
      (setq end-pos (+ end-pos pos))
      (if (not (= end-pos pos))
	  (setq ret-list (append ret-list (list
					   (downcase
					    (substring strin pos end-pos))))))
      (setq pos (+ end-pos 1))) ret-list))

(defun dun-listify-string2 (strin)
  (let (pos ret-list end-pos)
    (setq pos 0)
    (setq ret-list nil)
    (while (setq end-pos (string-match " " (substring strin pos)))
      (setq end-pos (+ end-pos pos))
      (if (not (= end-pos pos))
	  (setq ret-list (append ret-list (list
					   (downcase
					    (substring strin pos end-pos))))))
      (setq pos (+ end-pos 1))) ret-list))

(defun dun-replace (list n number)
  (rplaca (nthcdr n list) number))


;;; Get the first non-ignored word from a list.

(defun dun-firstword (list)
  (if (not (car list))
      nil
    (while (and list (member (intern (car list)) dun-ignore))
      (setq list (cdr list)))
    (car list)))

(defun dun-firstwordl (list)
  (if (not (car list))
      nil
    (while (and list (member (intern (car list)) dun-ignore))
      (setq list (cdr list)))
    list))

;;; parse a line passed in as a string  Call the proper verb with the
;;; rest of the line passed in as a list.

(defun dun-vparse (dun-ignore dun-verblist line)
  (dun-mprinc "\n")
  (setq line-list (dun-listify-string (concat line " ")))
  (dun-doverb dun-ignore dun-verblist (car line-list) (cdr line-list)))

(defun dun-parse2 (dun-ignore dun-verblist line)
  (dun-mprinc "\n")
  (setq line-list (dun-listify-string2 (concat line " ")))
  (dun-doverb dun-ignore dun-verblist (car line-list) (cdr line-list)))

;;; Read a line, in window mode

(defun dun-read-line ()
  (let (line)
    (setq line (read-string ""))
    (dun-mprinc line) line))

;;; Insert something into the window buffer

(defun dun-minsert (string)
  (if (stringp string)
      (insert string)
    (insert (prin1-to-string string))))

;;; Print something out, in window mode

(defun dun-mprinc (string)
  (if (stringp string)
      (insert string)
    (insert (prin1-to-string string))))

;;; In window mode, keep screen from jumping by keeping last line at
;;; the bottom of the screen.

(defun dun-fix-screen ()
  (interactive)
  (forward-line (- 0 (- (window-height) 2 )))
  (set-window-start (selected-window) (point))
  (goto-char (point-max)))

;;; Insert something into the buffer, followed by newline.

(defun dun-minsertl (string)
  (dun-minsert string)
  (dun-minsert "\n"))

;;; Print something, followed by a newline.

(defun dun-mprincl (string)
  (dun-mprinc string)
  (dun-mprinc "\n"))

;;; Function which will get an object number given the list of
;;; words in the command, except for the verb.

(defun dun-objnum-from-args (obj)
  (let (objnum)
    (setq obj (dun-firstword obj))
    (if (not obj)
	obj-special
      (setq objnum (cdr (assq (intern obj) dun-objnames))))))

(defun dun-objnum-from-args-std (obj)
  (let (result)
  (if (eq (setq result (dun-objnum-from-args obj)) obj-special)
      (dun-mprincl "You must supply an object."))
  (if (eq result nil)
      (dun-mprincl "I don't know what that is."))
  (if (eq result obj-special)
      nil
    result)))

;;; Is a string a member of a string list?

(defun dun-members (string string-list)
  (let (found)
    (setq found nil)
    (dolist (x string-list)
      (if (string= x string)
	  (setq found t))) found))
(setq dun-dos-verbs '((dir . dun-dos-dir) (type . dun-dos-type)
		      (exit . dun-dos-exit) (command . dun-dos-spawn)
		      (b: . dun-dos-invd) (c: . dun-dos-invd)
		      (a: . dun-dos-nil)))

(setq dun-verblist '((die . dun-die) (ne . dun-ne) (north . dun-n)
		     (south . dun-s) (east . dun-e) (west . dun-w)
		     (u . dun-up) (d . dun-down) (i . dun-inven)
		     (inventory . dun-inven) (look . dun-examine) (n . dun-n)
		     (s . dun-s) (e . dun-e) (w . dun-w) (se . dun-se)
		     (nw . dun-nw) (sw . dun-sw) (up . dun-up)
		     (down . dun-down) (in . dun-in) (out . dun-out)
		     (go . dun-go) (drop . dun-drop) (southeast . dun-se)
		     (southwest . dun-sw) (northeast . dun-ne)
		     (northwest . dun-nw) (save . dun-save-game)
		     (restore . dun-restore) (long . dun-long) (dig . dun-dig)
		     (shake . dun-shake) (wave . dun-shake)
		     (examine . dun-examine) (describe . dun-examine)
		     (climb . dun-climb) (eat . dun-eat) (put . dun-put)
		     (type . dun-type)  (insert . dun-put)
		     (score . dun-score) (help . dun-help) (quit . dun-quit)
		     (read . dun-examine) (verbose . dun-long)
		     (urinate . dun-piss) (piss . dun-piss)
		     (flush . dun-flush) (sleep . dun-sleep) (lie . dun-sleep)
		     (x . dun-examine) (break . dun-break) (drive . dun-drive)
		     (board . dun-in) (enter . dun-in) (turn . dun-turn)
		     (press . dun-press) (push . dun-press) (swim . dun-swim)
		     (on . dun-in) (off . dun-out) (chop . dun-break)
		     (switch . dun-press) (cut . dun-break) (exit . dun-out)
		     (leave . dun-out) (reset . dun-power) (flick . dun-press)
		     (superb . dun-superb) (answer . dun-answer)
		     (throw . dun-drop) (l . dun-examine) (take . dun-take)
		     (get . dun-take) (feed . dun-feed)))

(setq dun-mode 'moby)

(defconst north 0)
;;; These are the descriptions for the negative numbered objects from
;;; dun-room-objects

;;; These are the descriptions the user gets when regular objects are
;;; examined.

;;;;
;;;; This section defines the save and restore game functions for dunnet.
;;;;



(provide 'dunnet)

;;; dunnet.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars lexical)
;; End:

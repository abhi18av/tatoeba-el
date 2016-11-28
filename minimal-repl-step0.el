(defgroup tatoeba nil
  :prefix "tat-"
  :group 'games)

(define-derived-mode tatoeba-mode text-mode "Tatoeba"

  )

(defun tat-parse (arg)
  "Function called when return is pressed"
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
          (if (equal line "abc")
              (princ "I don't understand that.\n")))

      (goto-char (point-max))
      (princ "\n")))
  )



(defun tatoeba ()
  (interactive)
  (switch-to-buffer "*tatoeba*")
  (tatoeba-mode))




;;; Function which takes a verb and a list of other words.  Calls proper
;;; function associated with the verb, and passes along the other words.

(defun dun-doverb (dun-ignore dun-verblist verb rest)
  (if (not verb)
      nil
    (if (member (intern verb) dun-ignore)
	(if (not (car rest)) -1
	  (dun-doverb dun-ignore dun-verblist (car rest) (cdr rest)))
      (if (not (cdr (assq (intern verb) dun-verblist))) -1
	(setq dun-numcmds (1+ dun-numcmds))
	(eval (list (cdr (assq (intern verb) dun-verblist)) (quote rest)))))))


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




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(setq prompt "=> ")
(setq prompt-strings '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k"))



;; (with-current-buffer "x"
;;   (setf (buffer-string) " ")
;;   (insert (nth (random (length prompt-strings)) prompt-strings))
;;   )

;; (dotimes (i 10)
;; (with-current-buffer "x"
;;   (insert " " (nth (random (length prompt-strings)) prompt-strings))
;;   ))


(with-current-buffer "x"
  (setf (buffer-string) " ")
  (insert (nth (random (length prompt-strings)) prompt-strings))
  (insert prompt)
  (goto-char (point-max))

  (progn
    (setq key-read 0)
    (while (/= key-read 13)
      (setq key-read (read-key))
  (insert (string key-read))
))

;; this portion will happen when carriage-return is pressed
;;  (message (buffer-string))


  )


;; (defun analyzer (arg)
;;   (prin1-to-string (arg)))


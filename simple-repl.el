(defvar simple-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\n" 'simple-repl-read-print)
    (define-key map "\r" 'simple-repl-ret-or-read)
    map))

(define-derived-mode simple-repl-mode text-mode "simple-repl"
  "Major mode for running the Doctor (Eliza) program.
Like Text mode with Auto Fill mode
except that RET when point is after a newline, or LFD at any time,
reads the sentence before point, and prints the Doctor's answer."
  (make-simple-repl-variables)
  (turn-on-auto-fill)
  (insert "I am Eliza - the psychotherapist. Please describe your problems and
		 each time you are finished talking type RET twice")
  (insert "\n"))


;; Define equivalence classes of words that get treated alike.

;;;###autoload
(defun simple-repl ()
  "Switch to *doctor* buffer and start giving psychotherapy."
  (interactive)
  (switch-to-buffer "*simple-repl*")
  (simple-repl-mode))

(defun simple-repl-ret-or-read (arg)
  "Insert a newline if preceding character is not a newline.
Otherwise call the Doctor to parse preceding sentence."
  (interactive "*p")
  (if (= (preceding-char) ?\n)
      (simple-repl-read-print)
    (newline arg)))

(defun simple-repl-read-print ()
  "Top level loop."
  (interactive)
  (setq simple-repl-sent (simple-repl-readin))
  (insert "\n")
  (setq simple-repl--lincount (1+ simple-repl--lincount))
  (simple-repl-doc)
  (insert "\n")
  (setq simple-repl--bak simple-repl-sent))

(defun simple-repl-readin ()
  "Read a sentence.  Return it as a list of words."
  (let (sentence)
    (backward-sentence 1)
    (while (not (eobp))
      (setq sentence (append sentence (list (simple-repl-read-token)))))
    sentence))

(defun simple-repl-read-token ()
  "Read one word from buffer."
  (prog1 (intern (downcase (buffer-substring (point)
					     (progn
					       (forward-word 1)
					       (point)))))
    (re-search-forward "\\Sw*")))

;; Main processing function for sentences that have been read.

(defun simple-repl-doc ()
  (cond
   ((equal simple-repl-sent '(foo))
    (simple-repl-type '(bar! (doc$ simple-repl--please) (doc$ simple-repl--continue) \.)))



   ((member simple-repl-sent simple-repl--howareyoulst)
    (simple-repl-type '(i\'m ok \.  (doc$ simple-repl--describe) yourself \.)))


   ((or (member simple-repl-sent '((good bye) (see you later) (i quit) (so long)
			      (go away) (get lost)))
	(memq (car simple-repl-sent)
	      '(bye halt break quit done exit goodbye
		    bye\, stop pause goodbye\, stop pause)))
    (simple-repl-type (doc$ simple-repl--bye)))


   ((and (eq (car simple-repl-sent) 'you)
	 (memq (cadr simple-repl-sent) simple-repl--abusewords))
    (setq simple-repl-found (cadr simple-repl-sent))
    (simple-repl-type (doc$ simple-repl--abuselst)))


   ((eq (car simple-repl-sent) 'whatmeans)
    (simple-repl-def (cadr simple-repl-sent)))


   ((equal simple-repl-sent '(parse))
    (simple-repl-type (list  'subj '= simple-repl-subj ",  "
			'verb '= simple-repl-verb "\n"
			'object 'phrase '= simple-repl-obj ","
			'noun 'form '=  simple-repl-object "\n"
			'current 'keyword 'is simple-repl-found
			", "
			'most 'recent 'possessive
			'is simple-repl-owner "\n"
			'sentence 'used 'was
			"..."
			'(doc// simple-repl--bak))))


   ((memq (car simple-repl-sent) '(are is do has have how when where who why))
    (simple-repl-type (doc$ simple-repl--qlist)))


   ;;   ((eq (car sent) 'forget)
   ;;    (set (cadr sent) nil)
   ;;    (simple-repl-type '((doc$ simple-repl--isee) (doc$ simple-repl--please)
   ;;     (doc$ simple-repl--continue)\.)))

   (t

    (if (simple-repl-defq simple-repl-sent) (simple-repl-define simple-repl-sent simple-repl-found))

    (if (> (length simple-repl-sent) 12)
	(setq simple-repl-sent (simple-repl-shorten simple-repl-sent)))


    (setq simple-repl-sent (simple-repl-correct-spelling
		       (simple-repl-replace simple-repl-sent simple-repl--replist)))

    (cond

     ((and (not (memq 'me simple-repl-sent)) (not (memq 'i simple-repl-sent))
		(memq 'am simple-repl-sent))
	   (setq simple-repl-sent (simple-repl-replace simple-repl-sent '((am . (are)))))))


    (cond

     ((equal (car simple-repl-sent) 'yow) (simple-repl-zippy))

	  ((< (length simple-repl-sent) 2)
	   (cond ((eq (simple-repl-meaning (car simple-repl-sent)) 'howdy)
		  (simple-repl-howdy))
           (t (simple-repl-short))))

	  (t
	   (if (memq 'am simple-repl-sent)
	       (setq simple-repl-sent (simple-repl-replace simple-repl-sent '((me . (i))))))

     (setq simple-repl-sent (simple-repl-fixup simple-repl-sent))

     (if (and (eq (car simple-repl-sent) 'do) (eq (cadr simple-repl-sent) 'not))
	       (cond

               ((zerop (random 3))
		      (simple-repl-type '(are you (doc$ simple-repl--afraidof) that \?)))

               ((zerop (random 2))
		      (simple-repl-type '(don\'t tell me what to do \. i am the
					    doctor here!))
		      (simple-repl-rthing))

               (t
		      (simple-repl-type '((doc$ simple-repl--whysay) that i shouldn\'t
				     (cddr simple-repl-sent)
				     \?))))
	     (simple-repl-go (simple-repl-wherego simple-repl-sent))))))))

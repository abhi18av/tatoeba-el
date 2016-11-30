
(define-derived-mode tatoeba-mode text-mode "Tatoeba")


(defun tatoeba ()
  (interactive)
  (switch-to-buffer "*tatoeba*")
  (tatoeba-mode))

(setq-local font-lock-mode nil)

(mapc
 (lambda (x) (
              insert (propertize " color " 'face (list :foreground x))))
 '("red" "green" "orange"))




(defun doctor-ret-or-read (arg)
  "Insert a newline if preceding character is not a newline.
Otherwise call the Doctor to parse preceding sentence."
  (interactive "*p")
  (if (= (preceding-char) ?\n)
      (doctor-read-print)
    (newline arg)))

(defun doctor-read-print ()
  "Top level loop."
  (interactive)
  (setq doctor-sent (doctor-readin))
  (insert "\n")
  (setq doctor--lincount (1+ doctor--lincount))
  (doctor-doc)
  (insert "\n")
  (setq doctor--bak doctor-sent))

(defun doctor-readin ()
  "Read a sentence.  Return it as a list of words."
  (let (sentence)
    (backward-sentence 1)
    (while (not (eobp))
      (setq sentence (append sentence (list (doctor-read-token)))))
    sentence))




(defun doctor-doc ()
  (cond
   ((equal doctor-sent '(foo))
    (doctor-type '(bar! (doc$ doctor--please) (doc$ doctor--continue) \.)))
   ((member doctor-sent doctor--howareyoulst)
    (doctor-type '(i\'m ok \.  (doc$ doctor--describe) yourself \.)))
   ((or (member doctor-sent '((good bye) (see you later) (i quit) (so long)
			      (go away) (get lost)))
	(memq (car doctor-sent)
	      '(bye halt break quit done exit goodbye
		    bye\, stop pause goodbye\, stop pause)))
    (doctor-type (doc$ doctor--bye)))
   ((and (eq (car doctor-sent) 'you)
	 (memq (cadr doctor-sent) doctor--abusewords))
    (setq doctor-found (cadr doctor-sent))
    (doctor-type (doc$ doctor--abuselst)))
   ((eq (car doctor-sent) 'whatmeans)
    (doctor-def (cadr doctor-sent)))
   ((equal doctor-sent '(parse))
    (doctor-type (list  'subj '= doctor-subj ",  "
			'verb '= doctor-verb "\n"
			'object 'phrase '= doctor-obj ","
			'noun 'form '=  doctor-object "\n"
			'current 'keyword 'is doctor-found
			", "
			'most 'recent 'possessive
			'is doctor-owner "\n"
			'sentence 'used 'was
			"..."
			'(doc// doctor--bak))))
   ((memq (car doctor-sent) '(are is do has have how when where who why))
    (doctor-type (doc$ doctor--qlist)))
   ;;   ((eq (car sent) 'forget)
   ;;    (set (cadr sent) nil)
   ;;    (doctor-type '((doc$ doctor--isee) (doc$ doctor--please)
   ;;     (doc$ doctor--continue)\.)))
   (t
    (if (doctor-defq doctor-sent) (doctor-define doctor-sent doctor-found))
    (if (> (length doctor-sent) 12)
	(setq doctor-sent (doctor-shorten doctor-sent)))
    (setq doctor-sent (doctor-correct-spelling
		       (doctor-replace doctor-sent doctor--replist)))
    (cond ((and (not (memq 'me doctor-sent)) (not (memq 'i doctor-sent))
		(memq 'am doctor-sent))
	   (setq doctor-sent (doctor-replace doctor-sent '((am . (are)))))))
    (cond ((equal (car doctor-sent) 'yow) (doctor-zippy))
	  ((< (length doctor-sent) 2)
	   (cond ((eq (doctor-meaning (car doctor-sent)) 'howdy)
		  (doctor-howdy))
		 (t (doctor-short))))
	  (t
	   (if (memq 'am doctor-sent)
	       (setq doctor-sent (doctor-replace doctor-sent '((me . (i))))))
	   (setq doctor-sent (doctor-fixup doctor-sent))
	   (if (and (eq (car doctor-sent) 'do) (eq (cadr doctor-sent) 'not))
	       (cond ((zerop (random 3))
		      (doctor-type '(are you (doc$ doctor--afraidof) that \?)))
		     ((zerop (random 2))
		      (doctor-type '(don\'t tell me what to do \. i am the
					    doctor here!))
		      (doctor-rthing))
		     (t
		      (doctor-type '((doc$ doctor--whysay) that i shouldn\'t
				     (cddr doctor-sent)
				     \?))))
	     (doctor-go (doctor-wherego doctor-sent))))))))


(provide 'tatoeba)
 

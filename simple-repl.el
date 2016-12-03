
(defvar simple-repl--*print-space*)
(defvar simple-repl--*print-upcase*)
(defvar simple-repl--lincount)
(defvar simple-repl-found)

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
;    ((eq (first simple-repl-sent) 'you)

;    (memq (second simple-repl-sent) simple-repl--abusewords))




    ((and (eq (first simple-repl-sent) 'you)
          (memq (cadr simple-repl-sent) simple-repl--abusewords))
     (setq simple-repl-found (second simple-repl-sent))
     (simple-repl-type (simple-repl$ simple-repl--abuselst)))


     (t
      (insert "the default dialogue from the doctor"))))




 (defun make-simple-repl-variables ()
   (set (make-local-variable 'simple-repl-sent) nil)
   (set (make-local-variable 'doctor-found) nil)
    (set (make-local-variable 'simple-repl--lincount) 0)
     (set (make-local-variable 'simple-repl--abusewords)
          '(boring bozo clown clumsy cretin dumb dummy
                   fool foolish gnerd gnurd idiot jerk
                   lose loser louse lousy luse luser
                   moron nerd nurd oaf oafish reek
                   stink stupid tool toolish twit)))



 (defun simple-repl-type (x)
   (insert x))



;;  (defun simple-repl-type (x)
;;    (setq x (simple-repl-fix-2 x))
;;    (simple-repl-txtype (simple-repl-assm x)))


;;  (defun simple-repl-txtype (ans)
;;    "Output to buffer a list of symbols or strings as a sentence."
;;    (setq simple-repl--*print-upcase* t simple-repl--*print-space* nil)
;;    (mapc 'simple-repl-type-symbol ans)
;;    (insert "\n"))

;; (defun simple-repl-type-symbol (word)
;;  "Output a symbol to the buffer with some fancy case and spacing hacks."
;;  (setq word (simple-repl-make-string word))
;;  (if (string-equal word "i") (setq word "I"))
;;  (when simple-repl--*print-upcase*
;;    (setq word (capitalize word))
;;    (if simple-repl--*print-space* (insert " ")))
;;  (cond ((or (string-match "^[.,;:?! ]" word)
;;             (not simple-repl--*print-space*))
;;         (insert word))
;;        (t (insert ?\s word)))
;;  (and auto-fill-function
;;       (> (current-column) fill-column)
;;       (apply auto-fill-function nil))
;;  (setq simple-repl--*print-upcase* (string-match "[.?!]$" word)
;;        simple-repl--*print-space* t))



;; (defun simple-repl-make-string (obj)
;;   (cond ((stringp obj) obj)
;;         ((symbolp obj) (symbol-name obj))
;;         ((numberp obj) (int-to-string obj))
;;         (t "")))

;; ;;;; the part is self-contained

;;  (defun simple-repl-assm (proto)
;;    (cond ((null proto) nil)
;;          ((atom proto) (list proto))
;;          ((atom (car proto))
;;           (cons (car proto) (simple-repl-assm (cdr proto))))
;;          (t (simple-repl-concat (simple-repl-assm (eval (car proto))) (simple-repl-assm (cdr proto))))))


;;  (defun simple-repl-concat (x y)
;;    "Like append, but force atomic arguments to be lists."
;;    (append
;;     (if (and x (atom x)) (list x) x)
;;     (if (and y (atom y)) (list y) y)))



 ;;;;;;;;;; 
 ;; STUDY CAREFULLY

 ;; (defun doctor-doc ()
 ;;  (cond

 ;;   ((equal doctor-sent '(foo))
 ;;    (doctor-type '(bar! (doc$ doctor--please) (doc$ doctor--continue) \.)))


 ;;   ((member doctor-sent doctor--howareyoulst)
 ;;    (doctor-type '(i\'m ok \.  (doc$ doctor--describe) yourself \.)))


 ;;   ((or (member doctor-sent '((good bye) (see you later) (i quit) (so long)
 ;;            (go away) (get lost)))
 ;;  (memq (car doctor-sent)
 ;;        '(bye halt break quit done exit goodbye
 ;;        bye\, stop pause goodbye\, stop pause)))
 ;;    (doctor-type (doc$ doctor--bye)))



 ;;   ((and (eq (car doctor-sent) 'you)
 ;;   (memq (cadr doctor-sent) doctor--abusewords))
 ;;    (setq doctor-found (cadr doctor-sent))
 ;;    (doctor-type (doc$ doctor--abuselst)))


 ;;   ((eq (car doctor-sent) 'whatmeans)
 ;;    (doctor-def (cadr doctor-sent)))


 ;;   ((equal doctor-sent '(parse))
 ;;    (doctor-type (list  'subj '= doctor-subj ",  "
 ;;      'verb '= doctor-verb "\n"
 ;;      'object 'phrase '= doctor-obj ","
 ;;      'noun 'form '=  doctor-object "\n"
 ;;      'current 'keyword 'is doctor-found
 ;;      ", "
 ;;      'most 'recent 'possessive
 ;;      'is doctor-owner "\n"
 ;;      'sentence 'used 'was
 ;;      "..."
 ;;      '(doc// doctor--bak))))


 ;;   ((memq (car doctor-sent) '(are is do has have how when where who why))
 ;;    (doctor-type (doc$ doctor--qlist)))
 ;;   ;;   ((eq (car sent) 'forget)
 ;;   ;;    (set (cadr sent) nil)
 ;;   ;;    (doctor-type '((doc$ doctor--isee) (doc$ doctor--please)
 ;;   ;;     (doc$ doctor--continue)\.)))


 ;;   (t
 ;;    (if (doctor-defq doctor-sent) (doctor-define doctor-sent doctor-found))

 ;;    (if (> (length doctor-sent) 12)
 ;;  (setq doctor-sent (doctor-shorten doctor-sent)))


 ;;    (setq doctor-sent (doctor-correct-spelling
 ;;           (doctor-replace doctor-sent doctor--replist)))


 ;;    (cond ((and (not (memq 'me doctor-sent)) (not (memq 'i doctor-sent))
 ;;    (memq 'am doctor-sent))
 ;;     (setq doctor-sent (doctor-replace doctor-sent '((am . (are)))))))

 ;;    (cond ((equal (car doctor-sent) 'yow) (doctor-zippy))
 ;;    ((< (length doctor-sent) 2)

 ;;     (cond ((eq (doctor-meaning (car doctor-sent)) 'howdy)
 ;;      (doctor-howdy))
 ;;     (t (doctor-short))))

 ;;    (t
 ;;     (if (memq 'am doctor-sent)
 ;;         (setq doctor-sent (doctor-replace doctor-sent '((me . (i))))))
 ;;     (setq doctor-sent (doctor-fixup doctor-sent))
 ;;     (if (and (eq (car doctor-sent) 'do) (eq (cadr doctor-sent) 'not))
 ;;         (cond ((zerop (random 3))
 ;;          (doctor-type '(are you (doc$ doctor--afraidof) that \?)))
 ;;         ((zerop (random 2))
 ;;          (doctor-type '(don\'t tell me what to do \. i am the
 ;;              doctor here!))
 ;;          (doctor-rthing))
 ;;         (t
 ;;          (doctor-type '((doc$ doctor--whysay) that i shouldn\'t
 ;;             (cddr doctor-sent)
 ;;             \?))))
 ;;       (doctor-go (doctor-wherego doctor-sent))))))))


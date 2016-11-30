(defun doc// (x) x)

(defmacro doc$ (what)
  "Quoted arg form of doctor-$."
  `(doctor-$ ',what))

(defun doctor-$ (what)
  "Return the car of a list, rotating the list each time."
  (let* ((vv (symbol-value what))
	(first (car vv))
	(ww (append (cdr vv) (list first))))
    (set what ww)
    first))

(defvar doctor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\n" 'doctor-read-print)
    (define-key map "\r" 'doctor-ret-or-read)
    map))

(define-derived-mode doctor-mode text-mode "Doctor"
  "Major mode for running the Doctor (Eliza) program.
Like Text mode with Auto Fill mode
except that RET when point is after a newline, or LFD at any time,
reads the sentence before point, and prints the Doctor's answer."
  (make-doctor-variables)
  (turn-on-auto-fill)
  (doctor-type '(i am the psychotherapist \.
		 (doc$ doctor--please) (doc$ doctor--describe) your (doc$ doctor--problems) \.
		 each time you are finished talking\, type \R\E\T twice \.))
  (insert "\n"))


;; Define equivalence classes of words that get treated alike.

;;;###autoload
(defun doctor ()
  "Switch to *doctor* buffer and start giving psychotherapy."
  (interactive)
  (switch-to-buffer "*doctor*")
  (doctor-mode))

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

(defun doctor-read-token ()
  "Read one word from buffer."
  (prog1 (intern (downcase (buffer-substring (point)
					     (progn
					       (forward-word 1)
					       (point)))))
    (re-search-forward "\\Sw*")))

;; Main processing function for sentences that have been read.

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

;; Things done to process sentences once read.

(defun doctor-correct-spelling (sent)
  "Correct the spelling and expand each word in sentence."
  (if sent
      (apply 'append (mapcar (lambda (word)
				(if (memq word doctor--typos)
				    (get (get word 'doctor-correction)
					 'doctor-expansion)
				  (list word)))
			     sent))))

(defun doctor-shorten (sent)
  "Make a sentence manageably short using a few hacks."
  (let (foo
	(retval sent)
	(temp '(because but however besides anyway until
		    while that except why how)))
    (while temp
	   (setq foo (memq (car temp) sent))
	   (if (and foo
		    (> (length foo) 3))
	       (setq retval (doctor-fixup foo)
		     temp nil)
	       (setq temp (cdr temp))))
    retval))

(defun doctor-define (sent found)
  (doctor-svo sent found 1 nil)
  (and
   (doctor-nounp doctor-subj)
   (not (doctor-pronounp doctor-subj))
   doctor-subj
   (doctor-meaning doctor-object)
   (put doctor-subj 'doctor-meaning (doctor-meaning doctor-object))
   t))

(defun doctor-defq (sent)
  "Set global var DOCTOR-FOUND to first keyword found in sentence SENT."
  (setq doctor-found nil)
  (let ((temp '(means applies mean refers refer related
		      similar defined associated linked like same)))
    (while temp
	   (if (memq (car temp) sent)
	       (setq doctor-found (car temp)
		     temp nil)
	       (setq temp (cdr temp)))))
  doctor-found)

(defun doctor-def (x)
  (doctor-type (list 'the 'word x 'means (doctor-meaning x) 'to 'me))
  nil)

(defun doctor-forget ()
  "Delete the last element of the history list."
  (setq doctor--history (reverse (cdr (reverse doctor--history)))))

(defun doctor-query (x)
  "Prompt for a line of input from the minibuffer until a noun or verb is seen.
Put dialogue in buffer."
  (let (a
	(prompt (concat (doctor-make-string x)
			" what ?  "))
	retval)
    (while (not retval)
	   (while (not a)
	     (insert ?\n
		     prompt
		     (read-string prompt)
		     ?\n)
	     (setq a (doctor-readin)))
	   (while (and a (not retval))
		  (cond ((doctor-nounp (car a))
			 (setq retval (car a)))
			((doctor-verbp (car a))
			 (setq retval (doctor-build
				       (doctor-build x " ")
				       (car a))))
			((setq a (cdr a))))))
    retval))

(defun doctor-subjsearch (sent key type)
  "Search for the subject of a sentence SENT, looking for the noun closest
to and preceding KEY by at least TYPE words.  Set global variable doctor-subj to
the subject noun, and return the portion of the sentence following it."
  (let ((i (- (length sent) (length (memq key sent)) type)))
    (while (and (> i -1) (not (doctor-nounp (nth i sent))))
      (setq i (1- i)))
    (cond ((> i -1)
	   (setq doctor-subj (nth i sent))
	   (nthcdr (1+ i) sent))
	  (t
	   (setq doctor-subj 'you)
	   nil))))

(defun doctor-nounp (x)
  "Return t if the symbol argument is a noun."
	(or (doctor-pronounp x)
	    (not (or (doctor-verbp x)
		     (equal x 'not)
		     (doctor-prepp x)
		     (doctor-modifierp x) )) ))

(defun doctor-pronounp (x)
  "Return t if the symbol argument is a pronoun."
  (memq x '(
	i me mine myself
	we us ours ourselves ourself
	you yours yourself yourselves
	he him himself she hers herself
	it that those this these things thing
	they them themselves theirs
	anybody everybody somebody
	anyone everyone someone
	anything something everything)))

(dolist (x
         '(abort aborted aborts ask asked asks am
           applied applies apply are associate
           associated ate
           be became become becomes becoming
           been being believe believed believes
           bit bite bites bore bored bores boring bought buy buys buying
           call called calling calls came can caught catch come
           contract contracted contracts control controlled controls
           could croak croaks croaked cut cuts
           dare dared define defines dial dialed dials did die died dies
           dislike disliked
           dislikes do does drank drink drinks drinking
           drive drives driving drove dying
           eat eating eats expand expanded expands
           expect expected expects expel expels expelled
           explain explained explains
           fart farts feel feels felt fight fights find finds finding
           forget forgets forgot fought found
           fuck fucked fucking fucks
           gave get gets getting give gives go goes going gone got gotten
           had harm harms has hate hated hates have having
           hear heard hears hearing help helped helping helps
           hit hits hope hoped hopes hurt hurts
           implies imply is
           join joined joins jump jumped jumps
           keep keeping keeps kept
           kill killed killing kills kiss kissed kisses kissing
           knew know knows
           laid lay lays let lets lie lied lies like liked likes
           liking listen listens
           login look looked looking looks
           lose losing lost
           love loved loves loving
           luse lusing lust lusts
           made make makes making may mean means meant might
           move moved moves moving must
           need needed needs
           order ordered orders ought
           paid pay pays pick picked picking picks
           placed placing prefer prefers put puts
           ran rape raped rapes
           read reading reads recall receive received receives
           refer refered referred refers
           relate related relates remember remembered remembers
           romp romped romps run running runs
           said sang sat saw say says
           screw screwed screwing screws scrod see sees seem seemed
           seems seen sell selling sells
           send sendind sends sent shall shoot shot should
           sing sings sit sits sitting sold studied study
           take takes taking talk talked talking talks tell tells telling
           think thinks
           thought told took tooled touch touched touches touching
           transfer transferred transfers transmit transmits transmitted
           type types types typing
           walk walked walking walks want wanted wants was watch
           watched watching went were will wish would work worked works
           write writes writing wrote use used uses using))
  (put x 'doctor-sentence-type 'verb))

(defun doctor-verbp (x) (if (symbolp x)
			    (eq (get x 'doctor-sentence-type) 'verb)))

(defun doctor-plural (x)
  "Form the plural of the word argument."
  (let ((foo (doctor-make-string x)))
    (cond ((string-equal (substring foo -1) "s")
	   (cond ((string-equal (substring foo -2 -1) "s")
		  (intern (concat foo "es")))
		 (t x)))
	   ((string-equal (substring foo -1) "y")
	    (intern (concat (substring foo 0 -1)
			    "ies")))
	   (t (intern (concat foo "s"))))))

(defun doctor-setprep (sent key)
  (let ((val)
	(foo (memq key sent)))
    (cond ((doctor-prepp (cadr foo))
	   (setq val (doctor-getnoun (cddr foo)))
	   (cond (val val)
		 (t 'something)))
	  ((doctor-articlep (cadr foo))
	   (setq val (doctor-getnoun (cddr foo)))
	   (cond (val (doctor-build (doctor-build (cadr foo) " ") val))
		 (t 'something)))
	  (t 'something))))

(defun doctor-getnoun (x)
  (cond ((null x) (setq doctor-object 'something))
	((atom x) (setq doctor-object x))
	((eq (length x) 1)
	 (setq doctor-object (cond
		       ((doctor-nounp (setq doctor-object (car x))) doctor-object)
		       (t (doctor-query doctor-object)))))
	((eq (car x) 'to)
	 (doctor-build 'to\  (doctor-getnoun (cdr x))))
	((doctor-prepp (car x))
	 (doctor-getnoun (cdr x)))
	((not (doctor-nounp (car x)))
	 (doctor-build (doctor-build (cdr (assq (car x)
						(append
						 '((a . this)
						   (some . this)
						   (one . that))
						 (list
						  (cons
						   (car x) (car x))))))
				     " ")
		       (doctor-getnoun (cdr x))))
	(t (setq doctor-object (car x))
	   (doctor-build (doctor-build (car x) " ") (doctor-getnoun (cdr x))))
	))

(defun doctor-modifierp (x)
  (or (doctor-adjectivep x)
      (doctor-adverbp x)
      (doctor-othermodifierp x)))

(defun doctor-adjectivep (x)
  (or (numberp x)
      (doctor-nmbrp x)
      (doctor-articlep x)
      (doctor-colorp x)
      (doctor-sizep x)
      (doctor-possessivepronounp x)))

(defun doctor-adverbp (xx)
  (let ((xxstr (doctor-make-string xx)))
    (and (>= (length xxstr) 2)
	 (string-equal (substring (doctor-make-string xx) -2) "ly")
	 (not (memq xx '(family fly jelly rally))))))

(defun doctor-articlep (x)
  (memq x '(the a an)))

(defun doctor-nmbrp (x)
  (memq x '(one two three four five six seven eight nine ten
		eleven twelve thirteen fourteen fifteen
		sixteen seventeen eighteen nineteen
		twenty thirty forty fifty sixty seventy eighty ninety
		hundred thousand million billion
		half quarter
		first second third fourth fifth
		sixth seventh eighth ninth tenth)))

(defun doctor-colorp (x)
  (memq x '(beige black blue brown crimson
		  gray grey green
		  orange pink purple red tan tawny
		  violet white yellow)))

(defun doctor-sizep (x)
  (memq x '(big large tall fat wide thick
		small petite short thin skinny)))

(defun doctor-possessivepronounp (x)
  (memq x '(my your his her our their)))

(defun doctor-othermodifierp (x)
  (memq x '(all also always amusing any anyway associated awesome
		bad beautiful best better but certain clear
		ever every fantastic fun funny
		good great grody gross however if ignorant
		less linked losing lusing many more much
		never nice obnoxious often poor pretty real related rich
		similar some stupid super superb
		terrible terrific too total tubular ugly very)))

(defun doctor-prepp (x)
  (memq x '(about above after around as at
		  before beneath behind beside between by
		  for from in inside into
		  like near next of on onto over
		  same through thru to toward towards
		  under underneath with without)))

(defun doctor-remember (thing)
  (cond ((null doctor--history)
	 (setq doctor--history (list thing)))
	(t (setq doctor--history (append doctor--history (list thing))))))

(defun doctor-type (x)
  (setq x (doctor-fix-2 x))
  (doctor-txtype (doctor-assm x)))

(defun doctor-fixup (sent)
  (setq sent (append
	      (cdr
	       (assq (car sent)
		     (append
		      '((me  i)
			(him  he)
			(her  she)
			(them  they)
			(okay)
			(well)
			(sigh)
			(hmm)
			(hmmm)
			(hmmmm)
			(hmmmmm)
			(gee)
			(sure)
			(great)
			(oh)
			(fine)
			(ok)
			(no))
		      (list (list (car sent)
				  (car sent))))))
	      (cdr sent)))
  (doctor-fix-2 sent))

(defun doctor-fix-2 (sent)
  (let ((foo sent))
    (while foo
      (if (and (eq (car foo) 'me)
	       (doctor-verbp (cadr foo)))
	  (rplaca foo 'i)
	(cond ((eq (car foo) 'you)
	       (cond ((memq (cadr foo) '(am be been is))
		      (rplaca (cdr foo) 'are))
		     ((memq (cadr foo) '(has))
		      (rplaca (cdr foo) 'have))
		     ((memq (cadr foo) '(was))
		      (rplaca (cdr foo) 'were))))
	      ((equal (car foo) 'i)
	       (cond ((memq (cadr foo) '(are is be been))
		      (rplaca (cdr foo) 'am))
		     ((memq (cadr foo) '(were))
		      (rplaca (cdr foo) 'was))
		     ((memq (cadr foo) '(has))
		      (rplaca (cdr foo) 'have))))
	      ((and (doctor-verbp (car foo))
		    (eq (cadr foo) 'i)
		    (not (doctor-verbp (car (cddr foo)))))
	       (rplaca (cdr foo) 'me))
	      ((and (eq (car foo) 'a)
		    (doctor-vowelp (string-to-char
				    (doctor-make-string (cadr foo)))))
	       (rplaca foo 'an))
	      ((and (eq (car foo) 'an)
		    (not (doctor-vowelp (string-to-char
					 (doctor-make-string (cadr foo))))))
	       (rplaca foo 'a)))
	(setq foo (cdr foo))))
    sent))

(defun doctor-vowelp (x)
  (memq x '(?a ?e ?i ?o ?u)))

(defun doctor-replace (sent rlist)
  "Replace any element of SENT that is the car of a replacement
element pair in RLIST."
  (apply 'append
	 (mapcar
	   (lambda (x)
	     (cdr (or (assq x rlist)   ; either find a replacement
		      (list x x))))    ; or fake an identity mapping
	   sent)))

(defun doctor-wherego (sent)
  (cond ((null sent) (doc$ doctor--whereoutp))
	((null (doctor-meaning (car sent)))
	 (doctor-wherego (cond ((zerop (random 2))
				(reverse (cdr sent)))
			       (t (cdr sent)))))
	(t
	 (setq doctor-found (car sent))
	 (doctor-meaning (car sent)))))

(defun doctor-svo (sent key type mem)
  "Find subject, verb and object in sentence SENT with focus on word KEY.
TYPE is number of words preceding KEY to start looking for subject.
MEM is t if results are to be put on Doctor's memory stack.
Return in the global variables DOCTOR-SUBJ, DOCTOR-VERB, DOCTOR-OBJECT,
and DOCTOR-OBJ."
  (let ((foo (doctor-subjsearch sent key type)))
    (or foo
	(setq foo sent
	      mem nil))
    (while (and (null (doctor-verbp (car foo))) (cdr foo))
      (setq foo (cdr foo)))
    (setq doctor-verb (car foo))
    (setq doctor-obj (doctor-getnoun (cdr foo)))
    (cond ((eq doctor-object 'i) (setq doctor-object 'me))
	  ((eq doctor-subj 'me) (setq doctor-subj 'i)))
    (cond (mem (doctor-remember (list doctor-subj doctor-verb doctor-obj))))))

(defun doctor-possess (sent key)
  "Set possessive in SENT for keyword KEY.
Hack on previous word, setting global variable DOCTOR-OWNER to correct result."
  (let* ((i (- (length sent) (length (memq key sent)) 1))
	 (prev (if (< i 0) 'your
		 (nth i sent))))
    (setq doctor-owner
	  (if (or (doctor-possessivepronounp prev)
		  (string-equal "s"
				(substring (doctor-make-string prev)
					   -1)))
	      prev
	    'your))))

;; Output of replies.

(defun doctor-txtype (ans)
  "Output to buffer a list of symbols or strings as a sentence."
  (setq doctor--*print-upcase* t doctor--*print-space* nil)
  (mapc 'doctor-type-symbol ans)
  (insert "\n"))

(defun doctor-type-symbol (word)
  "Output a symbol to the buffer with some fancy case and spacing hacks."
  (setq word (doctor-make-string word))
  (if (string-equal word "i") (setq word "I"))
  (when doctor--*print-upcase*
    (setq word (capitalize word))
    (if doctor--*print-space* (insert " ")))
  (cond ((or (string-match "^[.,;:?! ]" word)
	     (not doctor--*print-space*))
	 (insert word))
	(t (insert ?\s word)))
  (and auto-fill-function
       (> (current-column) fill-column)
       (apply auto-fill-function nil))
  (setq doctor--*print-upcase* (string-match "[.?!]$" word)
	doctor--*print-space* t))

(defun doctor-build (str1 str2)
  "Make a symbol out of the concatenation of the two non-list arguments."
  (cond ((null str1) str2)
	((null str2) str1)
	((and (atom str1)
	      (atom str2))
	 (intern (concat (doctor-make-string str1)
			 (doctor-make-string str2))))
	(t nil)))

(defun doctor-make-string (obj)
  (cond ((stringp obj) obj)
	((symbolp obj) (symbol-name obj))
	((numberp obj) (int-to-string obj))
	(t "")))

(defun doctor-concat (x y)
  "Like append, but force atomic arguments to be lists."
  (append
   (if (and x (atom x)) (list x) x)
   (if (and y (atom y)) (list y) y)))

(defun doctor-assm (proto)
  (cond ((null proto) nil)
	((atom proto) (list proto))
	((atom (car proto))
	 (cons (car proto) (doctor-assm (cdr proto))))
	(t (doctor-concat (doctor-assm (eval (car proto))) (doctor-assm (cdr proto))))))

;; Functions that handle specific words or meanings when found.

(defun doctor-go (destination)
  "Call a `doctor-*' function."
  (funcall (intern (concat "doctor-" (doctor-make-string destination)))))

(defun doctor-desire1 ()
  (doctor-go (doc$ doctor--whereoutp)))

(defun doctor-huh ()
  (cond ((< (length doctor-sent) 9) (doctor-type (doc$ doctor--huhlst)))
	(t (doctor-type (doc$ doctor--longhuhlst)))))

(defun doctor-rthing () (doctor-type (doc$ doctor--thlst)))

(defun doctor-remem () (cond ((null doctor--history) (doctor-huh))
			     ((doctor-type (doc$ doctor--remlst)))))

(defun doctor-howdy ()
  (cond ((not doctor--howdyflag)
	 (doctor-type '((doc$ doctor--hello) what brings you to see me \?))
	 (setq doctor--howdyflag t))
	(t
	 (doctor-type '((doc$ doctor--ibelieve) we\'ve introduced ourselves already \.))
	 (doctor-type '((doc$ doctor--please) (doc$ doctor--describe) (doc$ doctor--things) \.)))))

(defun doctor-when ()
  (cond ((< (length (memq doctor-found doctor-sent)) 3) (doctor-short))
	(t
	 (setq doctor-sent (cdr (memq doctor-found doctor-sent)))
	 (setq doctor-sent (doctor-fixup doctor-sent))
	 (doctor-type '((doc$ doctor--whatwhen) (doc// doctor-sent) \?)))))

(defun doctor-conj ()
  (cond ((< (length (memq doctor-found doctor-sent)) 4) (doctor-short))
	(t
	 (setq doctor-sent (cdr (memq doctor-found doctor-sent)))
	 (setq doctor-sent (doctor-fixup doctor-sent))
	 (cond ((eq (car doctor-sent) 'of)
		(doctor-type '(are you (doc$ doctor--sure) that is the real reason \?))
		(setq doctor--things (cons (cdr doctor-sent) doctor--things)))
	       (t
		(doctor-remember doctor-sent)
		(doctor-type (doc$ doctor--beclst)))))))

(defun doctor-short ()
  (cond ((= (car doctor--repetitive-shortness) (1- doctor--lincount))
	 (rplacd doctor--repetitive-shortness
		 (1+ (cdr doctor--repetitive-shortness))))
	(t
	 (rplacd doctor--repetitive-shortness 1)))
  (rplaca doctor--repetitive-shortness doctor--lincount)
  (cond ((> (cdr doctor--repetitive-shortness) 6)
	 (cond ((not doctor--**mad**)
		(doctor-type '((doc$ doctor--areyou)
			       just trying to see what kind of things
			       i have in my vocabulary \? please try to
			       carry on a reasonable conversation!))
		(setq doctor--**mad** t))
	       (t
		(doctor-type '(i give up \. you need a lesson in creative
				 writing \.\.\.))
		)))
	(t
	 (cond ((equal doctor-sent (doctor-assm '(yes)))
		(doctor-type '((doc$ doctor--isee) (doc$ doctor--inter) (doc$ doctor--whysay) this is so \?)))
	       ((equal doctor-sent (doctor-assm '(because)))
		(doctor-type (doc$ doctor--shortbeclst)))
	       ((equal doctor-sent (doctor-assm '(no)))
		(doctor-type (doc$ doctor--neglst)))
	       (t (doctor-type (doc$ doctor--shortlst)))))))

(defun doctor-alcohol () (doctor-type (doc$ doctor--drnk)))

(defun doctor-desire ()
  (let ((foo (memq doctor-found doctor-sent)))
    (cond ((< (length foo) 2)
	   (doctor-go (doctor-build (doctor-meaning doctor-found) 1)))
	  ((memq (cadr foo) '(a an))
	   (rplacd foo (append '(to have) (cdr foo)))
	   (doctor-svo doctor-sent doctor-found 1 nil)
	   (doctor-remember (list doctor-subj 'would 'like doctor-obj))
	   (doctor-type (doc$ doctor--whywant)))
	  ((not (eq (cadr foo) 'to))
	   (doctor-go (doctor-build (doctor-meaning doctor-found) 1)))
	  (t
	   (doctor-svo doctor-sent doctor-found 1 nil)
	   (doctor-remember (list doctor-subj 'would 'like doctor-obj))
	   (doctor-type (doc$ doctor--whywant))))))

(defun doctor-drug ()
  (doctor-type (doc$ doctor--drugs))
  (doctor-remember (list 'you 'used doctor-found)))

(defun doctor-toke ()
  (doctor-type (doc$ doctor--toklst)))

(defun doctor-state ()
  (doctor-type (doc$ doctor--states)) (doctor-remember (list 'you 'were doctor-found)))

(defun doctor-mood ()
  (doctor-type (doc$ doctor--moods)) (doctor-remember (list 'you 'felt doctor-found)))

(defun doctor-fear ()
  (setq doctor--feared (doctor-setprep doctor-sent doctor-found))
  (doctor-type (doc$ doctor--fears))
  (doctor-remember (list 'you 'were 'afraid 'of doctor--feared)))

(defun doctor-hate ()
  (doctor-svo doctor-sent doctor-found 1 t)
  (cond ((memq 'not doctor-sent) (doctor-forget) (doctor-huh))
	((equal doctor-subj 'you)
	 (doctor-type '(why do you (doc// doctor-verb) (doc// doctor-obj) \?)))
	(t (doctor-type '((doc$ doctor--whysay) (list doctor-subj doctor-verb doctor-obj))))))

(defun doctor-symptoms ()
  (doctor-type '((doc$ doctor--maybe) you should consult a medical doctor\;
		 i am a psychotherapist. \.)))

(defun doctor-hates ()
  (doctor-svo doctor-sent doctor-found 1 t)
  (doctor-hates1))

(defun doctor-hates1 ()
  (doctor-type '((doc$ doctor--whysay) (list doctor-subj doctor-verb doctor-obj) \?)))

(defun doctor-loves ()
  (doctor-svo doctor-sent doctor-found 1 t)
  (doctor-qloves))

(defun doctor-qloves ()
  (doctor-type '((doc$ doctor--bother) (list doctor-subj doctor-verb doctor-obj) \?)))

(defun doctor-love ()
  (doctor-svo doctor-sent doctor-found 1 t)
  (cond ((memq 'not doctor-sent) (doctor-forget) (doctor-huh))
	((memq 'to doctor-sent) (doctor-hates1))
	(t
	 (cond ((equal doctor-object 'something)
		(setq doctor-object '(this person you love))))
	 (cond ((equal doctor-subj 'you)
		(setq doctor--lover doctor-obj)
		(cond ((equal doctor--lover '(this person you love))
		       (setq doctor--lover '(your partner))
		       (doctor-forget)
		       (doctor-type '(with whom are you in love \?)))
		      ((doctor-type '((doc$ doctor--please)
				      (doc$ doctor--describe)
				      (doc$ doctor--relation)
				      (doc// doctor--lover)
				      \.)))))
	       ((equal doctor-subj 'i)
		(doctor-txtype '(we were discussing you!)))
	       (t (doctor-forget)
		  (setq doctor-obj 'someone)
		  (setq doctor-verb (doctor-build doctor-verb 's))
		  (doctor-qloves))))))

(defun doctor-mach ()
  (setq doctor-found (doctor-plural doctor-found))
  (doctor-type (doc$ doctor--machlst)))

(defun doctor-sexnoun () (doctor-sexverb))

(defun doctor-sexverb ()
  (if (or (memq 'me doctor-sent) (memq 'myself doctor-sent) (memq 'i doctor-sent))
      (doctor-foul)
    (doctor-type (doc$ doctor--sexlst))))

(defun doctor-death ()
  (cond (doctor--suicide-flag (doctor-type (doc$ doctor--deathlst)))
	((or (equal doctor-found 'suicide)
             (and (or (equal doctor-found 'kill)
                      (equal doctor-found 'killing))
                  (memq 'yourself doctor-sent)))
	 (setq doctor--suicide-flag t)
	 (doctor-type '(If you are really suicidal\, you might
			   want to contact the Samaritans via
			   E-mail: jo@samaritans.org or\, at your option\,
			   anonymous E-mail: samaritans@anon.twwells.com\ \.
                           or find a Befrienders crisis center at
			   http://www.befrienders.org/\ \.
			   (doc$ doctor--please) (doc$ doctor--continue) \.)))
	(t (doctor-type (doc$ doctor--deathlst)))))

(defun doctor-foul ()
  (doctor-type (doc$ doctor--foullst)))

(defun doctor-family ()
  (doctor-possess doctor-sent doctor-found)
  (doctor-type (doc$ doctor--famlst)))

;; I did not add this -- rms.
;; But he might have removed it.  I put it back.  --roland
(defun doctor-rms ()
  (cond (doctor--rms-flag (doctor-type (doc$ doctor--stallmanlst)))
	(t (setq doctor--rms-flag t) (doctor-type '(do you know Stallman \?)))))

(defun doctor-school nil (doctor-type (doc$ doctor--schoollst)))

(defun doctor-eliza ()
  (cond (doctor--eliza-flag (doctor-type (doc$ doctor--elizalst)))
	(t (setq doctor--eliza-flag t)
	   (doctor-type '((doc// doctor-found) \? hah !
			  (doc$ doctor--please) (doc$ doctor--continue) \.)))))

(defun doctor-sports () (doctor-type (doc$ doctor--sportslst)))

(defun doctor-math () (doctor-type (doc$ doctor--mathlst)))

(defun doctor-zippy ()
  (cond (doctor--zippy-flag (doctor-type (doc$ doctor--zippylst)))
	(t (setq doctor--zippy-flag t)
	   (doctor-type '(yow! are we interactive yet \?)))))


(defun doctor-chat () (doctor-type (doc$ doctor--chatlst)))

(provide 'doctor)

;;; doctor.el ends here

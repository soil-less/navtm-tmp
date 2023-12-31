;;; nav-thing-meow --- meow things hint and expand -*- lexical-binding:t; -*-

;;; Commentary:

;; Import file after loading meow

;;; Code:

;;; Strings:
(defun navtm--thing-next-string (direction inner-bounds)
  "Return the bounds of the next string in direction.

   DIRECTION can either be `forward' or `backward'.
   INNER-BOUNDS can either be `inner' or `bounds'."
  (let* (
    (forward (eq direction 'forward))
    (inner (eq inner-bounds 'inner))
    (start-bounds (bounds-of-thing-at-point 'string))
    (start (if start-bounds
	       (if forward (cdr start-bounds) (- (car start-bounds) 1))
	     (point))))
    (goto-char start)
    (while (and
      (not (bounds-of-thing-at-point 'string))
      (not (= 0 (if forward
		    (skip-syntax-forward "^\"|")
                  (+ (skip-syntax-backward "^\"|")
		     (skip-syntax-backward "\"|")))))))
    (let ((bounds (bounds-of-thing-at-point 'string)))
      (if (and bounds inner)
	  (cons (+ (car bounds) 1) (- (cdr bounds) 1))
	bounds))))

(defun navtm--thing-next-inner-string ()
  "Return the bounds of the next inner string."
  (navtm--thing-next-string 'forward 'inner))

(defun navtm--thing-prev-inner-string ()
  "Return the bounds of the previous inner string."
  (navtm--thing-next-string 'backward 'inner))

(defun navtm--thing-next-bounds-string ()
  "Return the bounds of the next string."
  (navtm--thing-next-string 'forward 'bounds))

(defun navtm--thing-prev-bounds-string ()
  "Return the bounds of the previous string."
  (navtm--thing-next-string 'backward 'bounds))

;;; Defuns:
(defun navtm--thing-next-defun (direction)
  "Return the point/bounds of the next defun in direction.

   DIRECTION can either be `forward' or `backward'."
  (save-mark-and-excursion
    (let* (
      (start-bounds (bounds-of-thing-at-point 'defun))
      (start (if (eq direction 'forward)
                 (if start-bounds (+ (cdr start-bounds) 1) (point))
               (if start-bounds (- (car start-bounds) 1) (point))))
      (search-count (if (eq direction 'forward) '1 '-1)))
      (goto-char start)
      (while (and
        (not (bounds-of-thing-at-point 'defun))
        (not (eq (point) (progn (forward-line search-count) (point))))))
      (let ((bounds (bounds-of-thing-at-point 'defun)))
        (when bounds
  	(cons (car bounds) (- (cdr bounds) 1)))))))

(defun navtm--thing-inner-defun ()
  "Bounds of thing at point defun."
  (bounds-of-thing-at-point 'defun))

(defun navtm--thing-prev-inner-defun ()
  "Prev inner defun."
  (navtm--thing-next-defun 'backward))

(defun navtm--thing-next-inner-defun ()
  "Next inner defun."
  (navtm--thing-next-defun 'forward))

;;; Paragraphs
(defun navtm--thing-next-paragraph (direction)
  "Return the point/bounds of the next paragraph in direction.

   DIRECTION can either be `forward' or `backward'."
  (save-mark-and-excursion
    (let* (
      (start-bounds (bounds-of-thing-at-point 'paragraph))
      (start (if (eq direction 'forward)
                 (if start-bounds (+ (cdr start-bounds) 1) (point))
               (if start-bounds (- (car start-bounds) 1) (point))))
      (search-count (if (eq direction 'forward) '1 '-1)))
      (goto-char start)
      (while (and
        (not (bounds-of-thing-at-point 'paragraph))
        (not (eq (point) (progn (forward-line search-count) (point))))))
      (let ((bounds (bounds-of-thing-at-point 'paragraph)))
	(when bounds
          (let ((bounds-beg (if (eq (car bounds) 1) 1 (+ (car bounds) 1)))
		(bounds-end (cdr bounds)))
	    (cons bounds-beg bounds-end)))))))

(defun navtm--thing-next-inner-paragraph ()
  "Return the point or the bounds of the next inner paragraph."
  (let ((bounds (navtm--thing-next-paragraph 'forward)))
    (when bounds (cons (car bounds) (cdr bounds)))))

(defun navtm--thing-prev-inner-paragraph ()
  "Return the point or the bounds of the previous inner paragraph."
  (let ((bounds (navtm--thing-next-paragraph 'backward)))
    (when bounds (cons (car bounds) (cdr bounds)))))

(defun navtm--thing-inner-paragraph ()
  "Select the current inner paragraph."
  (let ((bounds (bounds-of-thing-at-point 'paragraph)))
    (when bounds
      (let ((bounds-beg (if (eq (car bounds) 1) 1 (+ (car bounds) 1)))
	    (bounds-end (cdr bounds)))
	  (cons bounds-beg bounds-end)))))

(defun navtm--thing-next-bounds-paragraph ()
  "Return bounds of the next paragraph."
  (let ((bounds (navtm--thing-next-paragraph 'forward)))
    (when bounds (cons (car bounds) (+ (cdr bounds) 1)))))

(defun navtm--thing-prev-bounds-paragraph ()
  "Return the bounds of the previous paragraph."
  (let ((bounds (navtm--thing-next-paragraph 'backward)))
    (when bounds (cons (car bounds) (+ (cdr bounds) 1)))))

(defun navtm--thing-bounds-paragraph ()
  "Select the current paragraph."
  (let ((bounds (bounds-of-thing-at-point 'paragraph)))
    (when bounds
      (let ((bounds-beg (if (eq (car bounds) 1) 1 (+ (car bounds) 1)))
	    (bounds-end (+ (cdr bounds) 1)))
	  (cons bounds-beg bounds-end)))))

;;; Pairs:
(defun navtm--thing-next-pair-function (direction push pop inner
					    &optional search-pop)
  "Return the next inner/bounds pair in direction.

   PUSH is the regexp for the opening pair.
   POP is the regexp for the closing pair.
   INNER is t for inner of pair, nil for bounds of pair.
   DIRECTION is either `forward' or `backward'.
   If SEARCH-POP is non-nil searches for the next pair using pop-token instead
   of push-token."
  (save-mark-and-excursion
    (let*
     ((forward (eq direction 'forward))
      (start-bounds
        (progn
          (when (and forward (not inner) (not search-pop))
	    (goto-char (+ (point) 1)))
          (meow--thing-pair-function push pop t)))
      (match-offset (if forward (if search-pop '-1 '0) (if search-pop '0 '1)))
      (search-count (if forward '1 '-1))
      (token (if search-pop pop push)))
     (while
       (and
         (re-search-forward token nil t search-count)
	 (not (save-mark-and-excursion
	        (goto-char (+ (point) match-offset))
	        (let ((found-bounds (meow--thing-pair-function push pop t)))
	             (and found-bounds
		          (or (eq (point) (car found-bounds))
		              (eq (point) (cdr found-bounds)))
		          (not (equal found-bounds start-bounds))))))))
     (goto-char (+ (point) match-offset))
     (let (( bounds (meow--thing-pair-function push pop t)))
       (when (and bounds (not (eq bounds start-bounds)))
           (if inner
               bounds
             (cons (- (car bounds) 1) (+ (cdr bounds) 1))))))))

(defun navtm--thing-make-pair-function (x near)
  "Return list of function for pair X.

   X is a pair of string token lists, the tokens in the first list are used to
   find beginning, the token in second list are used to find end.
   If NEAR is non-nil we use the point of near end of match,
   otherwise point of far end of match.

   List returned is of the form :
   (pair-function prev-pair-function next-pair-function)."
  (let*
   ((push-token (let ((tokens (cadr x)))
                  (string-join (mapcar #'regexp-quote tokens) "\\|")))
    (pop-token (let ((tokens (caddr x)))
                 (string-join (mapcar #'regexp-quote tokens) "\\|")))
    (selection (lambda()
		 (meow--thing-pair-function push-token pop-token near)))
    (prev (lambda() (navtm--thing-next-pair-function
                      'backward push-token pop-token near)))
    (next (lambda() (navtm--thing-next-pair-function
                      'forward push-token pop-token near)))
    (startof (lambda() (car (navtm--thing-next-pair-function
			     'backward push-token pop-token near))))
    (endof (lambda() (cdr (navtm--thing-next-pair-function
			   'forward push-token pop-token near 'search-pop)))))
    (list selection prev next startof endof)))

;;; Syntax:
(defun navtm--thing-next-syntax-function (direction syntax)
  "Return the bounds of next SYNTAX in DIRECTION.

  DIRECTION can be `forward' or `backward'.
  SYNTAX is a cons cell of the form (syntax . SYNTAX-EXPR)."
  (save-mark-and-excursion
    (let* (
      (forward (eq direction 'forward))
      (match-syntax (cdr syntax))
      (miss-syntax (if (equal (substring match-syntax 0 1) "^")
    		     (substring match-syntax 1)
    		   (format "^%s" match-syntax)))
      (skip-syntax-fn (if forward #'skip-syntax-forward #'skip-syntax-backward))
    )
    (funcall skip-syntax-fn match-syntax)
    (funcall skip-syntax-fn miss-syntax)
    (let ((match-start (point)))
      (funcall skip-syntax-fn match-syntax)
      (when (not (eq match-start (point)))
        (if forward
            (cons match-start (point))
            (cons (point) match-start)))))))

(defun navtm--thing-make-syntax-function (x)
  "Return list of functions for syntax X.

   List returned is of the form :
   (syntax-function . (prev-syntax-function . next-syntax-function))."
  (let*
   ((selection (lambda () (meow--thing-syntax-function x)))
    (prev (lambda () (navtm--thing-next-syntax-function 'backward x)))
    (next (lambda () (navtm--thing-next-syntax-function 'forward x)))
    (startof (lambda() (car (funcall prev))))
    (endof (lambda() (cdr (funcall next)))))
  (list selection prev next startof endof)))

;;; Regexps:
(defun navtm--thing-next-regexp-function (direction b-re f-re near)
  "Return the bounds of next regexp in DIRECTION.

  DIRECTION can be `forward' or `backward'.
  B-RE is the regexp used for beginning.
  F-RE is the regexp used for end.
  If NEAR is non-nil we use the point of near end of match,
  otherwise point of far end of match."
  (save-mark-and-excursion
    (if (eq direction 'forward)
	(when (re-search-forward b-re nil t 1)
	  (goto-char (match-end 0))
	  (meow--thing-regexp-function b-re f-re near))
      (when
	(re-search-backward f-re nil t 1)
	(goto-char (match-beginning 0))
	(meow--thing-regexp-function b-re f-re near)))))

(defun navtm--thing-make-regexp-function (x near)
  "Return list of function for regexp X.

   X is a pair of regexp (b-re f-re) with b-re used for beginning and
   f-re used for end.
   If NEAR is non-nil we use the point of near end of match,
   otherwise point of far end of match.

   List returned is of the form :
   (regexp-function prev-regexp-function next-regexp-function)."
  (let*
   ((b-re (cadr x))
    (f-re (caddr x))
    (selection (lambda() (meow--thing-regexp-function b-re f-re near)))
    (prev (lambda()
	    (navtm--thing-next-regexp-function 'backward b-re f-re near)))
    (next (lambda()
	    (navtm--thing-next-regexp-function 'forward b-re f-re near)))
    (startof (lambda() (car (funcall prev))))
    (endof (lambda() (cdr (funcall next)))))
    (list selection prev next startof endof)))

;;; Symbols
(defun navtm--thing-next-symbol-function (direction x forward-op)
  "Return bounds of next symbol X in DIRECTION using function FORWARD-OP."
  (save-mark-and-excursion
    (let
	((count (if (eq direction 'forward) '1 '-1))
	 (start-bounds (bounds-of-thing-at-point x)))
      (when start-bounds
	(if (eq direction 'forward)
            (goto-char (+ (cdr start-bounds) 1))
          (goto-char (- (car start-bounds) 1))))
      (funcall forward-op count)
      (bounds-of-thing-at-point x))))

(defun navtm--thing-make-symbol-function (x)
  "Return list of function for symbol X.

   List returned is of the form :
   (symbol-function prev-symbol-function next-symbol-function).
   If symbol X has neither a forward-op property or a function named
   forward-X, a lambda returning nil is used instead."
  (let ((forward-op (or (get x 'forward-op)
			(intern-soft (format "forward-%s" x)))))
    (unless (functionp forward-op)
      (message "meow-thing-register:\
                No forward-op for symbol %s and no forward-%s found.\
                Navigation/expansions to %s will not be available.\
                Consider registering %s as functions." x x x x))
    (let*
     ((selection (lambda() (bounds-of-thing-at-point x)))
      (prev (if (functionp forward-op)
	        (lambda()
		  (navtm--thing-next-symbol-function 'backward x forward-op))
	      (lambda() 'nil)))
      (next (if (functionp forward-op)
	        (lambda()
		  (navtm--thing-next-symbol-function 'forward x forward-op))
	      (lambda() 'nil)))
      (startof (lambda() (car (funcall prev))))
      (endof (lambda() (cdr (funcall next)))))
    (list selection prev next startof endof))))

;;; Function
(defun navtm--thing-make-function-function (x)
  "Return list of function for function X.

   List returned is of the form :
   (select-function prev-function next-function)."
  (let*
   ((prev-next (eq (length x) 4))
    (startof-endof (eq (length x) 6))
    (selection (if (eq (length x) 1) (nth 0 x) (nth 1 x)))
    (prev (if prev-next (nth 2 x) (lambda() 'nil)))
    (next (if prev-next (nth 3 x) (lambda() 'nil)))
    (startof (if startof-endof (nth 4 x) (lambda() (car (funcall prev)))))
    (endof (if startof-endof (nth 5 x) (lambda() (cdr (funcall next))))))
   (list selection prev next startof endof)))

;;; Registry
(defvar navtm--thing-registry nil
  "Thing registry.

This is a plist mapping from thing to :
\((inner-fn prev-inner-fn next-inner-fn)
  (bounds-fn prev-bounds-fn next-bounds-fn))

All of inner-fn, prev-inner-fn, next-inner-fn, bounds-fn, prev-bounds-fn and
next-bounds-fn return a cons of (start . end) for that thing.")

(defun navtm--thing-register (thing inner-fn-list bounds-fn-list)
  "Register INNER-FN-LIST and BOUNDS-FN-LIST to a THING."
  (setq navtm--thing-registry
        (plist-put navtm--thing-registry
                   thing
                   (list inner-fn-list bounds-fn-list))))

(defun navtm--parse-range-of-thing (thing inner)
  "Parse either inner or bounds of THING.

   If INNER is non-nil then parse inner."
  (when-let (bounds-fn-pair (plist-get navtm--thing-registry thing))
    (let ((function-list
	   (if inner (nth 0 bounds-fn-pair) (nth 1 bounds-fn-pair))))
      (funcall (nth 0 function-list)))))

(defun navtm--parse-prev-of-thing (thing inner)
  "Parse either inner or bounds of previous THING.

   If INNER is non-nil then parse inner."
  (when-let (bounds-fn-pair (plist-get navtm--thing-registry thing))
    (let ((function-list
	   (if inner (nth 0 bounds-fn-pair) (nth 1 bounds-fn-pair))))
        (funcall (nth 1 function-list)))))

(defun navtm--parse-next-of-thing (thing inner)
  "Parse either inner or bounds of next THING.

   If INNER is non-nil then parse inner."
  (when-let (bounds-fn-pair (plist-get navtm--thing-registry thing))
    (let ((function-list
	   (if inner (nth 0 bounds-fn-pair) (nth 1 bounds-fn-pair))))
        (funcall (nth 2 function-list)))))

(defun navtm--parse-start-of-thing (thing inner)
  "Parse until start of INNER THING."
  (when-let (bounds-fn-pair (plist-get navtm--thing-registry thing))
    (let ((function-list
	   (if inner (nth 0 bounds-fn-pair) (nth 1 bounds-fn-pair))))
        (funcall (nth 3 function-list)))))

(defun navtm--parse-end-of-thing (thing inner)
  "Parse until end of INNER THING."
  (when-let (bounds-fn-pair (plist-get navtm--thing-registry thing))
    (let ((function-list
	   (if inner (nth 0 bounds-fn-pair) (nth 1 bounds-fn-pair))))
        (funcall (nth 4 function-list)))))

(defun navtm--thing-parse (x near)
  "Parse thing X according to type.

  NEAR is non-nil for inner."
  (cond
   ((functionp x)
    (navtm--thing-make-function-function (list x)))
   ((symbolp x)
    (navtm--thing-make-symbol-function x))
   ((equal 'functions (car x))
    (navtm--thing-make-function-function x))
   ((equal 'syntax (car x))
    (navtm--thing-make-syntax-function x))
   ((equal 'regexp (car x))
    (navtm--thing-make-regexp-function x near))
   ((equal 'pair (car x))
    (navtm--thing-make-pair-function x near))
   ((listp x)
    (meow--thing-parse-multi x near))
   (t
    (list (lambda () (message "Meow: THING definition broken")
	             (cons (point) (point)))
	  (lambda () 'nil)
	  (lambda () 'nil)))))

(defun navtm-thing-register (thing inner bounds)
  "Temporary docstring THING INNER BOUNDS."
  (let ((inner-fn-list (navtm--thing-parse inner t))
        (bounds-fn-list (navtm--thing-parse bounds nil)))
    (navtm--thing-register thing inner-fn-list bounds-fn-list)))

;;; Things definition:

;; Paragraph cannot be implemented that way, (forward-paragraph -1) put the
;; point on the line after the previous paragraph, and
;; (bounds-of-thing-at-point 'paragraph) when inbetween paragraphs selects the
;; next one. We cannot (goto-char (- (point) 1)) because then the sentences
;; return incorrect results when searching backward.
;; (navtm-thing-register 'paragraph 'paragraph 'paragraph)

(navtm-thing-register 'sentence 'sentence 'sentence)
(navtm-thing-register
 'defun
 '(functions navtm--thing-inner-defun
	     navtm--thing-prev-inner-defun
	     navtm--thing-next-inner-defun)
 '(functions navtm--thing-inner-defun
	     navtm--thing-prev-inner-defun
	     navtm--thing-next-inner-defun))

(navtm-thing-register
 'paragraph
 '(functions navtm--thing-inner-paragraph
	     navtm--thing-prev-inner-paragraph
	     navtm--thing-next-inner-paragraph)
 '(functions navtm--thing-bounds-paragraph
	     navtm--thing-prev-bounds-paragraph
	     navtm--thing-next-bounds-paragraph))

(navtm-thing-register 'round '(pair ("(") (")")) '(pair ("(") (")")))
(navtm-thing-register 'square '(pair ("[") ("]")) '(pair ("[") ("]")))
(navtm-thing-register 'curly '(pair ("{") ("}")) '(pair ("{") ("}")))
(navtm-thing-register 'angle '(pair ("<") (">")) '(pair ("<") (">")))

;; (navtm-thing-register 'non-whitespace '(syntax . "^-") '(syntax . "^-"))

;; (navtm-thing-register 'testangle '(regexp "<" ">") '(regexp "<" ">"))

(navtm-thing-register
 'string
 '(functions meow--inner-of-string
   navtm--thing-prev-inner-string navtm--thing-next-inner-string)
 '(functions meow--bounds-of-string
   navtm--thing-prev-bounds-string navtm--thing-next-bounds-string))

;; BELOW NOT CHANGED FROM FIRST IMPL, PLEASE DISREGARD FOR NOW
;;; Common:

(defun navtm--select-expandable-p ()
  "Return non-nil if selection is expandable."
  (when (meow-normal-mode-p)
    (when-let ((sel (meow--selection-type)))
      (let* ((type (cdr sel)))
        (alist-get type meow-expand-hint-counts nil nil #'equal)))))

;; (plist-get navtm--thing-registry 'string)

(defun navtm--is-selectable-thing-p ()
  "Return non-nil if SELECTION is an selectable thing."
  (let ((sel-type (meow--selection-type)))
    (and sel-type
	 (member (car sel-type) '(select-backward select-forward))
         (and meow--expand-nav-function
	    (car meow--expand-nav-function)
	    (cdr meow--expand-nav-function)))))

(defun navtm--is-expandable-thing-p ()
  "Return non-nil if SELECTION is an expandable thing."
  (let ((sel-type (meow--selection-type)))
    (and sel-type
         (plist-get navtm--thing-registry (cdr sel-type))
         (and meow--expand-nav-function
	    (car meow--expand-nav-function)
	    (cdr meow--expand-nav-function)))))

(defun navtm-reverse ()
  "Reverse hint directions or exchange point and mark.

  If the selection is an expandable thing, reverse hints direction,
  otherwise exchange point and mark.
  This command supports `meow-selection-command-fallback' ?"
  (interactive)
  (meow--with-selection-fallback
    (if (navtm--is-selectable-thing-p)
	(let*
	 ((sel meow--selection)
	  (sel-type (car (car sel)))
	  (sel-thing (cdr (car sel)))
	  (sel-start (car (cdr sel)))
	  (sel-end (cdr (cdr sel)))
	  (sel-type (if (eq sel-type 'select-backward)
			    'select-forward 'select-backward)))
	  (setq meow--selection
		(list (cons sel-type sel-thing) sel-start sel-end)))
      (meow--execute-kbd-macro meow--kbd-exchange-point-and-mark))
     (if (member last-command
               '(meow-visit meow-search meow-mark-symbol meow-mark-word))
       (meow--highlight-regexp-in-buffer (car regexp-search-ring))
     (navtm--maybe-highlight-num-positions))))

(setq meow-expand-hint-counts '(
  (word . 30) (line . 30) (block . 30) (find . 30) (till . 30) (string . 30)
  (round . 30) (square . 30) (curly . 30) (angle . 30) (defun . 30)
  (paragraph . 30) (sentence . 30)
))

(defun navtm--maybe-highlight-num-positions (&optional nav-functions)
  "Highlight hints if selection is an expandable thing.

  NAV-FUNCTIONS is a cons cell with car the function to call to find previous
  matches and cdr the function to call to find next matches."
  (when (navtm--select-expandable-p)
    (setq meow--expand-nav-function
	  (or nav-functions meow--expand-nav-function))
    (when (and (not (member major-mode meow-expand-exclude-mode-list))
               meow--expand-nav-function)
      (let ((num (alist-get (cdr (meow--selection-type))
			    meow-expand-hint-counts)))
        (navtm--highlight-num-positions num)))))

(defun navtm--highlight-num-positions (num)
  "Hihlight NUM positions."
  (setq meow--visual-command this-command)
  (meow--remove-expand-highlights)
  (meow--remove-match-highlights)
  (meow--remove-search-indicator)
  (let* ((bound (cons (window-start) (window-end)))
        (faces (seq-take
                (if (meow--direction-backward-p)
                    (seq-concatenate
                     'list
                     (make-list 10 'meow-position-highlight-reverse-number-1)
                     (make-list 10 'meow-position-highlight-reverse-number-2)
                     (make-list 10 'meow-position-highlight-reverse-number-3))
                  (seq-concatenate
                   'list
                   (make-list 10 'meow-position-highlight-number-1)
                   (make-list 10 'meow-position-highlight-number-2)
                   (make-list 10 'meow-position-highlight-number-3)))
                num))
	(backward (meow--direction-backward-p))
        (nav-function (if backward
                          (car meow--expand-nav-function)
                        (cdr meow--expand-nav-function)))
        (nav-function (cond
		       ((navtm--is-selectable-thing-p)
			(lambda() (goto-char (car (funcall nav-function)))))
		       ((and (navtm--is-expandable-thing-p) backward)
			(lambda() (goto-char (car (funcall nav-function)))))
		       ((navtm--is-expandable-thing-p)
			(lambda() (goto-char (car (funcall nav-function)))))
		       (t nav-function))))
    (meow--highlight-num-positions-1 nav-function faces bound)
    (when meow--highlight-timer
      (cancel-timer meow--highlight-timer)
      (setq meow--highlight-timer nil))
    (setq meow--highlight-timer
          (run-at-time
           (time-add (current-time)
                     (seconds-to-time meow-expand-hint-remove-delay))
           nil
           #'meow--remove-expand-highlights))))

(defun navtm--select-thing (back bounds thing)
  "BACK BOUNDS THING."
  (when bounds
    (let* ((sel-direction (if back 'select-backward 'select-forward))
	   (selection (cons sel-direction thing)))
      (thread-first
	(meow--make-selection selection (cdr bounds) (car bounds))
	(meow--select)))))

(defun navtm-inner-of-thing (thing)
  "Select inner (excluding delimiters) of THING."
  (interactive (list (meow-thing-prompt "Inner of: ")))
  (save-window-excursion
    (let ((back (equal 'backward (meow--thing-get-direction 'inner)))
          (bounds (navtm--parse-inner-of-thing-char thing))
	  (back 'nil))
      (navtm--select-thing back bounds
			   (cdr (assoc thing meow-char-thing-table)))
      (navtm--maybe-highlight-num-positions))))

(defun navtm-bounds-of-thing (thing)
  "Select bounds (including delimiters) of THING."
  (interactive (list (meow-thing-prompt "Bounds of: ")))
  (save-window-excursion
    (let ((back (equal 'backward (meow--thing-get-direction 'bounds)))
          (bounds (navtm--parse-bounds-of-thing-char thing))
	  (back 'nil))
      (navtm--select-thing back bounds
			   (cdr (assoc thing meow-char-thing-table)))
      (navtm--maybe-highlight-num-positions))))

(defun navtm-beginning-of-thing (thing)
  "Select to the beginning of THING."
  (interactive (list (meow-thing-prompt "Beginning of: ")))
  (save-window-excursion
    (let* ((back (equal 'backward (meow--thing-get-direction 'beginning)))
          (bounds (navtm--parse-inner-of-thing-char thing back)))
      (when bounds
        (thread-first
          (meow--make-selection (cons 'expand
				      (cdr (assoc thing meow-char-thing-table)))
                                (if back (point) (car bounds))
                                (if back (car bounds) (point)))
          (meow--select))
	(navtm--maybe-highlight-num-positions)))))

(defun navtm-end-of-thing (thing)
  "Select to the beginning of THING."
  (interactive (list (meow-thing-prompt "End of: ")))
  (save-window-excursion
    (let* ((back (equal 'backward (meow--thing-get-direction 'end)))
          (bounds (navtm--parse-inner-of-thing-char thing back)))
      (when bounds
        (thread-first
          (meow--make-selection (cons 'expand
				      (cdr (assoc thing meow-char-thing-table)))
                                (if back (cdr bounds) (point))
                                (if back (point) (cdr bounds)))
          (meow--select))
	(navtm--maybe-highlight-num-positions)))))

(defun navtm--thing-set-nav-functions (thing inner)
  "Set nav-functions corresponding to INNER THING."
  (let* ((thing-functions (plist-get navtm--thing-registry thing))
	 (function-list
	  (if inner (nth 0 thing-functions) (nth 1 thing-functions)))
	 (prev (nth 1 function-list))
	 (next (nth 2 function-list)))
    (setq meow--expand-nav-function (cons prev next))))

(defun navtm--parse-inner-of-thing-char (ch &optional backward)
  "Parse inner of thing corresponding to CH.

   If none is at point, return the next/previous one.
   If BACKWARD is non-nil, return previous, otherwise next."
  (when-let ((ch-to-thing (assoc ch meow-char-thing-table)))
    (let*
	((thing (cdr ch-to-thing))
	 (range-bounds (navtm--parse-range-of-thing thing t))
	 (next-bounds (if backward
                          (navtm--parse-prev-of-thing thing t)
                        (navtm--parse-next-of-thing thing t))))
      (cond
       ((and range-bounds
	     (eq (car range-bounds) (cdr range-bounds))
	     (not next-bounds))
	range-bounds)
       ((and range-bounds (not (eq (car range-bounds) (cdr range-bounds))))
	(navtm--thing-set-nav-functions thing t)
	range-bounds)
       (next-bounds
	(navtm--thing-set-nav-functions thing t)
	next-bounds)))))

(defun navtm--parse-bounds-of-thing-char (ch &optional backward)
  "Parse inner of thing correcponding to CH."
  (when-let ((ch-to-thing (assoc ch meow-char-thing-table)))
    (let*
	((thing (cdr ch-to-thing))
	 (range-bounds (navtm--parse-range-of-thing thing 'nil))
	 (next-bounds (if backward
			  (navtm--parse-prev-of-thing thing 'nil)
			(navtm--parse-next-of-thing thing 'nil))))
      (cond
       ((and range-bounds
	     (eq (car range-bounds) (cdr range-bounds))
	     (not next-bounds))
	range-bounds)
       ((and range-bounds (not (eq (car range-bounds) (cdr range-bounds))))
	(navtm--thing-set-nav-functions thing 'nil)
	range-bounds)
       (next-bounds
	(navtm--thing-set-nav-functions thing 'nil)
	next-bounds)))))

(defun navtm-expand-1 () "Expand 1." (interactive) (navtm-expand 1))
(defun navtm-expand-2 () "Expand 2." (interactive) (navtm-expand 2))
(defun navtm-expand-3 () "Expand 3." (interactive) (navtm-expand 3))
(defun navtm-expand-4 () "Expand 4." (interactive) (navtm-expand 4))
(defun navtm-expand-5 () "Expand 5." (interactive) (navtm-expand 5))
(defun navtm-expand-6 () "Expand 6." (interactive) (navtm-expand 6))
(defun navtm-expand-7 () "Expand 7." (interactive) (navtm-expand 7))
(defun navtm-expand-8 () "Expand 8." (interactive) (navtm-expand 8))
(defun navtm-expand-9 () "Expand 9." (interactive) (navtm-expand 9))
(defun navtm-expand-0 () "Expand 0." (interactive) (navtm-expand 0))
(defun navtm-expand (&optional n)
  "Expand selection by N matches or select the Nth next match.

   If selection is an expandable thing select the Nth next/previous match,
   otherwise call meow-expand."
  (interactive)
  (when (and meow--expand-nav-function
             (region-active-p)
             (meow--selection-type))
    (let* ((n (or n (string-to-number (char-to-string last-input-event))))
	   (n (if (= n 0) 10 n)))
      (if (not (navtm--is-selectable-thing-p))
          (meow-expand n)
        (let*
	 ((nav-function (if (meow--direction-backward-p)
                            (car meow--expand-nav-function)
                          (cdr meow--expand-nav-function)))
	  (sel-type (meow--selection-type))
	  (bounds
            (save-mark-and-excursion
	      (dotimes (_ (- n 1))
                (let ((temp-bounds (funcall nav-function)))
                   (when temp-bounds (goto-char (car temp-bounds)))))
              (funcall nav-function))))

        (when bounds
          (thread-first
	    (meow--make-selection sel-type
				  (cdr bounds)
				  (car bounds))
            (meow--select)))
	      (navtm--maybe-highlight-num-positions
	       meow--expand-nav-function))))))

;;; Overrides :

;; Those overrides are necessary because those functions are used
;; by meow itself when determining which direction should be
;; highlighted / expanded.

(defun meow--direction-backward-p ()
  "Return whether we have a backward selection."
    (and (region-active-p)
    (if (navtm--is-selectable-thing-p)
      (eq 'select-backward (car (meow--selection-type)))
      (> (mark) (point)))))

(defun meow--direction-forward-p ()
  "Return whether we have a forward selection."
    (and (region-active-p)
    (if (navtm--is-selectable-thing-p)
      (eq 'select-forward (car (meow--selection-type)))
      (<= (mark) (point)))))

(provide 'nav-thing-meow)
;;; nav-thing-meow.el ends here

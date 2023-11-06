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

(defun navtm--thing-next-inner-string (&optional return-bounds)
  "Return the point or the bounds of the next inner string.

   RETURN-BOUNDS if nil or omitted return point,
   if t return bounds."
  (let ((bounds (navtm--thing-next-string 'forward 'inner)))
    (if return-bounds bounds (car bounds))))

(defun navtm--thing-prev-inner-string (&optional return-bounds)
  "Return the point or the bounds of the previous inner string.

   RETURN-BOUNDS if nil or omitted return point,
   if t return bounds."
  (let ((bounds (navtm--thing-next-string 'backward 'inner)))
    (if return-bounds bounds (car bounds))))

(defun navtm--thing-next-bounds-string (&optional return-bounds)
  "Return the point or the bounds of the next string.

   RETURN-BOUNDS if nil or omitted return point,
   if t return bounds."
  (let ((bounds (navtm--thing-next-string 'forward 'bounds)))
    (if return-bounds bounds (car bounds))))

(defun navtm--thing-prev-bounds-string (&optional return-bounds)
  "Return the point or the bounds of the previous string.

   RETURN-BOUNDS if nil or omitted return point,
   if t return bounds."
  (let ((bounds (navtm--thing-next-string 'backward 'bounds)))
    (if return-bounds bounds (car bounds))))

;;; Pairs:
(defun navtm--thing-next-pair-function (direction push pop inner)
  "Return the next inner/bounds pair in direction.

   PUSH is the regexp for the opening pair.
   POP is the regexp for the closing pair.
   INNER is t for inner of pair, nil for bounds of pair.
   DIRECTION is either `forward' or `backward'.
   Moves the point to the beginning of the match if one is found."
  (save-mark-and-excursion
    (let* (
      (p (point))
      (start-bounds (progn
               (unless inner (goto-char (+ (point) 1)))
               (meow--thing-pair-function push pop inner)
      ))
      (start-offset
        (if (eq direction 'forward)
             ;; inner ; bounds
          (if inner '1 '2) ;; forward
          (if inner -1 '0) ;; backward
      ))
      (start (if start-bounds (+ (car start-bounds) start-offset) (point)))
      (search-count (if (eq direction 'forward) '1 '-1))
      (search-offset (if (eq direction 'forward) '0 '1)))
      (goto-char start)
      (while (and
        (not (eq (point) (car (meow--thing-pair-function push pop t))))
        (and (re-search-forward push nil t search-count)
             (goto-char (+ (point) search-offset)))))
      (let (( bounds (meow--thing-pair-function push pop t)))
        (when (and bounds (not (eq (car bounds) p)))
            (if inner
                bounds
              (cons (- (car bounds) 1) (+ (cdr bounds) 1))))))))

(defun navtm--thing-make-pair-function (x near)
  "Return cons cell of function for pair X.

   X is a pair of string token lists, the tokens in the first list are used to
   find beginning, the token in second list are used to find end.
   If NEAR is non-nil we use the point of near end of match,
   otherwise point of far end of match.

   Cons cell returned is of the form :
   (pair-function . (prev-pair-function . next-pair-function))."
  (let* ((push-token (let ((tokens (cadr x)))
                       (string-join (mapcar #'regexp-quote tokens) "\\|")))
         (pop-token (let ((tokens (caddr x)))
                      (string-join (mapcar #'regexp-quote tokens) "\\|"))))
    (cons
     (lambda () (meow--thing-pair-function push-token pop-token near))
     (cons
      (lambda () (navtm--thing-next-pair-function
		   'backward push-token pop-token near))
      (lambda () (navtm--thing-next-pair-function
		   'forward push-token pop-token near))))))

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
  "Return cons cell of function for syntax X.

   Cons cell returned is of the form :
   (syntax-function . (prev-syntax-function . next-syntax-function))."
  (cons
   (lambda () (meow--thing-syntax-function x))
   (cons
     (lambda () (navtm--thing-next-syntax-function 'backward x))
     (lambda () (navtm--thing-next-syntax-function 'forward x)))))

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
  "Return cons cell of function for regexp X.

   X is a pair of regexp (b-re f-re) with b-re used for beginning and
   f-re used for end.
   If NEAR is non-nil we use the point of near end of match,
   otherwise point of far end of match.

   Cons cell returned is of the form :
   (regexp-function . (prev-regexp-function . next-regexp-function))."
  (let* ((b-re (cadr x))
         (f-re (caddr x)))
  (cons
   (lambda () (meow--thing-regexp-function b-re f-re near))
   (cons
     (lambda () (navtm--thing-next-regexp-function 'backward b-re f-re near))
     (lambda () (navtm--thing-next-regexp-function 'forward b-re f-re near))))))

;;; Symbols
(defun navtm--thing-next-symbol-function (direction x forward-op)
  "Return bounds of next symbol X in DIRECTION using function FORWARD-OP."
  (save-mark-and-excursion
    (let ((count (if (eq direction 'forward) '1 '-1)))
      (funcall forward-op count)
      (bounds-of-thing-at-point x))))

(defun navtm--thing-make-symbol-function (x)
  "Return cons cell of function for symbol X.

   Cons cell returned is of the form :
   (symbol-function . (prev-symbol-function . next-symbol-function))."
  (let ((forward-op (or (get x 'forward-op)
			(intern-soft (format "forward-%s" x)))))
    (cons
      (lambda () (bounds-of-thing-at-point x))
      (if (functionp forward-op)
	  (cons
	   (lambda ()
	     (navtm--thing-next-symbol-function 'backward x forward-op))
	   (lambda ()
	     (navtm--thing-next-symbol-function 'forward x forward-op)))
        (progn
	  (display-warning
	    "Can't determine prev/next functions for thing %s" x)
          (cons 'nil 'nil))))))

;;; Function
(defun navtm--thing-make-function-function (x)
  "Return cons cell of function for function X.

   Cons cell returned is of the form :
   (select-function . (prev-function . next-function))."
  (let*
   ((prev-next (eq (length x) 4))
    (selection (nth 1 x))
    (prev (when prev-next (nth 2 x)))
    (next (when prev-next (nth 3 x))))
   (cons selection (cons prev next))))

;;; Registry
(defvar navtm--thing-registry nil
  "Thing registry.

This is a plist mapping from thing to :
\((inner-fn . (prev-inner-fn . next-inner-fn))
 . (bounds-fn . (prev-bounds-fn . next-bounds-fn)))

All of inner-fn, prev-inner-fn, next-inner-fn, bounds-fn, prev-bounds-fn and
next-bounds-fn return a cons of (start . end) for that thing.")

(defun navtm--thing-register (thing inner-fn bounds-fn)
  "Register INNER-FN and BOUNDS-FN to a THING."
  (setq navtm--thing-registry
        (plist-put navtm--thing-registry
                   thing
                   (cons inner-fn bounds-fn))))

(defun navtm--parse-range-of-thing (thing inner)
  "Parse either inner or bounds of THING.

   If INNER is non-nil then parse inner."
  (when-let (bounds-fn-pair (plist-get navtm--thing-registry thing))
    (if inner
        (funcall (car (car bounds-fn-pair)))
      (funcall (car (cdr bounds-fn-pair))))))

(defun navtm--parse-prev-of-thing (thing inner)
  "Parse either inner or bounds of previous THING.

   If INNER is non-nil then parse inner."
  (when-let (bounds-fn-pair (plist-get navtm--thing-registry thing))
    (if inner
        (funcall (car (cdr (car bounds-fn-pair))))
      (funcall (car (cdr (cdr bounds-fn-pair)))))))

(defun navtm--parse-next-of-thing (thing inner)
  "Parse either inner or bounds of next THING.

   If INNER is non-nil then parse inner."
  (when-let (bounds-fn-pair (plist-get navtm--thing-registry thing))
    (if inner
        (funcall (cdr (cdr (car bounds-fn-pair))))
      (funcall (cdr (cdr (cdr bounds-fn-pair)))))))

(defun navtm--thing-parse (x near)
  "Parse thing X according to type.

  NEAR is non-nil for inner."
  (cond
   ((functionp x)
    x)
   ((symbolp x)
    (navtm--thing-make-symbol-function x))
   ((equal 'function (car x))
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
    (lambda ()
      (message "Meow: THING definition broken")
      (cons (point) (point))))))

(defun navtm-thing-register (thing inner bounds)
  "Temporary docstring THING INNER BOUNDS."
      (let ((inner-fn (navtm--thing-parse inner t))
          (bounds-fn (navtm--thing-parse bounds nil)))
      (navtm--thing-register thing inner-fn bounds-fn)))

;;; Things definition:

;; Paragraph cannot be implemented that way, (forward-paragraph -1) put the
;; point on the line after the previous paragraph, and
;; (bounds-of-thing-at-point 'paragraph) when inbetween paragraphs selects the
;; next one. We cannot (goto-char (- (point) 1)) because then the sentences
;; return incorrect results when searching backward.
;; (navtm-thing-register 'paragraph 'paragraph 'paragraph)

;; (navtm-thing-register 'sentence 'sentence 'sentence)

;; (navtm-thing-register 'round '(pair ("(") (")")) '(pair ("(") (")")))
;; (navtm-thing-register 'square '(pair ("[") ("]")) '(pair ("[") ("]")))
;; (navtm-thing-register 'curly '(pair ("{") ("}")) '(pair ("{") ("}")))
;; (navtm-thing-register 'angle '(pair ("<") (">")) '(pair ("<") (">")))

;; (navtm-thing-register 'non-whitespace '(syntax . "^-") '(syntax . "^-"))

;; (navtm-thing-register 'testangle '(regexp "<" ">") '(regexp "<" ">"))

;; (navtm-thing-register
;;  'string
;;  '(function #'meow--inner-of-string
;;    #'navtm--thing-prev-inner-string #'navtm--thing-next-inner-string)
;;  '(function #'meow--bounds-of-string
;;    #'navtm--thing-prev-bounds-string #'navtm--thing-next-bounds-string))

;; BELOW NOT CHANGED FROM FIRST IMPL, PLEASE DISREGARD FOR NOW
;;; Common:

(defun navtm--select-expandable-p ()
  "Return non-nil if selection is expandable."
  (when (meow-normal-mode-p)
    (when-let ((sel (meow--selection-type)))
      (let ((type (cdr sel)))
        (or (member type navtm-expandable-types)
	    (member type navtm-expandable-things))))))

(defun navtm-reverse ()
  "Reverse hint directions or exchange point and mark.

  If the selection is an expandable thing, reverse hints direction,
  otherwise exchange point and mark.
  This command supports `meow-selection-command-fallback' ?"
  (interactive)
  (meow--with-selection-fallback
  (let ((sel (meow--selection-type)))
    (if (and sel (member (cdr sel) navtm-expandable-things))
      (if (and (bound-and-true-p navtm-thing-direction)
	           (eq 'forward navtm-thing-direction))
        (setq navtm-thing-direction 'backward)
        (setq navtm-thing-direction 'forward))
      (meow--execute-kbd-macro meow--kbd-exchange-point-and-mark))
     (if (member last-command
               '(meow-visit meow-search meow-mark-symbol meow-mark-word))
       (meow--highlight-regexp-in-buffer (car regexp-search-ring))
     (navtm--maybe-highlight-num-positions)))))

(setq navtm-expandable-types '(word line block find till))
(setq navtm-expandable-things '(string round square curly angle defun
				paragraph sentence))
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
        (meow--highlight-num-positions num)))))


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
    (let* ((n (or n (string-to-number (char-to-string last-input-event)))))
      (if (not (member (cdr (meow--selection-type)) navtm-expandable-things))
          (meow-expand n)
        (let* (
	      (nav-function (if (meow--direction-backward-p)
			       (car meow--expand-nav-function)
			       (cdr meow--expand-nav-function)))
	      (sel-type (cons meow-expand-selection-type
			      (cdr (meow--selection-type)))))
          (save-mark-and-excursion
	        (dotimes (_ n)
		  (setq +tempbounds (funcall nav-function 't))))
	      (when (not (eq nil +tempbounds))
          (thread-first
	    (meow--make-selection sel-type
				  (cdr +tempbounds)
				  (car +tempbounds))
            (meow--select)))
	      (navtm--maybe-highlight-num-positions
	       meow--expand-nav-function))))))

;;; Overrides :

;; Those overrides are necessary because those functions are used
;; by meow itself when determining which direction should be
;; highlighted / expanded.

(defun meow--direction-backward-p ()
  "Return whether we have a backward selection."
  (let ((sel (meow--selection-type)))
    (if (and sel (member (cdr sel) navtm-expandable-things))
      (and (region-active-p) (bound-and-true-p navtm-thing-direction)
	       (eq 'backward navtm-thing-direction))
      (and (region-active-p) (> (mark) (point))))))

(defun meow--direction-forward-p ()
  "Return whether we have a forward selection."
  (let ((sel (meow--selection-type)))
    (if (and sel (member (cdr sel) navtm-expandable-things))
      (and (region-active-p) (bound-and-true-p navtm-thing-direction)
	       (eq 'forward navtm-thing-direction))
      (and (region-active-p) (<= (mark) (point))))))



(provide 'nav-thing-meow)
;;; nav-thing-meow.el ends here

;;; flora.el --- a major mode for editing and running F-Logic programs

;; Adapted from flp.el by M. Kifer (kifer@cs.sunysb.edu)
;; Flp.el, a major mode for FLORID, was derived from prolog.el 
;; from GNU Emacs 19.28 

;; Authors:
;; Heinz Uphoff, uphoff@informatik.uni-freiburg.de
;; Christian Schlepphorst, schlepph@informatik.uni-freiburg.de

;;; Commentary:

;; This package provides a major mode for editing F-Logic.  It knows
;; about Flora syntax and comments (well, sort of), and can send
;; regions, buffers, and files to an inferior F-Logic interpreter
;; process.
;;
;; Put
;;   (setq auto-mode-alist (cons '("\\.flr$" . flora-mode) auto-mode-alist))
;;   (autoload 'flora-mode "flora" "Major mode for editing Flora programs." t)

;;; Code:

(require 'comint)

;; This path has to be set at installation of the F-Logic-System!!!
(defvar flora-program-name "xsb -e '[flora], flora_shell.'"
  "*Program name for invoking an inferior Flora with `run-flora'.")

(defvar flora-mode-syntax-table nil)
(defvar flora-mode-abbrev-table nil)
(defvar flora-mode-map nil)

(defvar flora-consult-string "[user].\n"
  "*Consult stdin as plain input. ")

(defvar flora-forget-string "halt.\n xsb -e '[flora], flora_shell.'\n"
  "*reinitialise  system")

(defvar flora-offer-save t
  "*If non-nil, ask about saving modified buffers before 
\\[flora-consult-file] is run.")

(defvar flora-indent-width 4)

(defvar flora-indent-mline-comments-flag t
  "*Non-nil means automatically align comments when indenting.")

(defconst flora-quoted-atom-regexp
  "\\(^\\|[^0-9]\\)\\('\\([^\n']\\|\\\\'\\)*'\\)"
;  "[^0-9]\\('\\([^\n']\\|\\\\'\\)*'\\)"
  "Regexp matching a quoted atom.")
(defconst flora-string-regexp
  "\\(\"\\([^\n\"]\\|\\\\\"\\)*\"\\)"
  "Regexp matching a string.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Do fontifification of F-logic-syntax with font-lock.
;; If font-lock is not installed, there should be no problem

(make-face 'flora-font-lock-system-face)
(make-face 'flora-font-lock-arrow-face)
(copy-face 'default 'flora-font-lock-arrow-face)
(set-face-foreground 'flora-font-lock-arrow-face "Maroon")
(copy-face 'default 'flora-font-lock-system-face)
(set-face-foreground 'flora-font-lock-system-face "violet")
(make-face-bold 'flora-font-lock-system-face)
(make-face-bold 'flora-font-lock-arrow-face)

(make-face 'flora-font-lock-query-face)
(set-face-foreground 'flora-font-lock-query-face "darkgreen")

(defconst flora-font-lock-keywords
   (list
    '("\\(\\?-\\|:-\\|\\.[ \t\n]*$\\)"
      1 'flora-font-lock-query-face)
    '("\\(\\*?->>\\|\\*?->\\|\\*?=>>\\|\\*?=>\\)"
      1 'flora-font-lock-arrow-face)
    ;; for objects
    '("\\([A-Za-z0-9][A-Za-z0-9!.]*\\) *\\["
      1 'font-lock-variable-name-face)
    '("\\b\\(not\\|avg\\|sum\\|count\\|collectset\\|collectbag\\|assert\\|retract\\|erase\\|retractall\\)\\b"
      1 'font-lock-keyword-face)
    '("\\(:\\|;\\)" 
      1 'font-lock-type-face)
    '("\\(\\[\\|\\]\\|{\\|}\\)"
      1 'bold)
    '("\\b\\(index\\|from\\|table\\|import\\|export\\)\\b"
      1 'flora-font-lock-system-face)
    '("\\(\\b[A-Za-z0-9]+\\b\\) *\\*?[---=]>"
      1 'font-lock-function-name-face)
    )
  "Additional expressions to highlight in flora mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if flora-mode-syntax-table
    ()
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?/ ". 1456" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?% "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?\' "\"" table)
    (setq flora-mode-syntax-table table)))


(define-abbrev-table 'flora-mode-abbrev-table ())

(defun flora-mode-variables ()
  (set-syntax-table flora-mode-syntax-table)
  (setq local-abbrev-table flora-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter)) ;;stolen from cplusplus 
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'flora-indent-line)
  (make-local-variable 'comment-start)
  (setq comment-start "%")
  (make-local-variable 'comment-start-skip)
  ;; This complex regexp makes sure that comments cannot start
  ;; inside quoted atoms or strings
  (setq comment-start-skip 
	(format "^\\(\\(%s\\|%s\\|[^\n\'\"%%]\\)*\\)\\(/\\*+ *\\|%%+ *\\)" 
		flora-quoted-atom-regexp flora-string-regexp))
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-column)
  (setq comment-column 48)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'flora-comment-indent)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
	'(flora-font-lock-keywords nil nil ((?_ . "w"))))
)

(defun flora-mode-commands (map)
  (define-key map "\t" 'flora-indent-line)
  (define-key map "\C-c\C-l" 'flora-switch-to-flora-buffer)
  (define-key map "\C-c\C-b" 'flora-consult-buffer)
  (define-key map "\t" 'comint-dynamic-complete)
  (define-key map "\C-c\C-r" 'flora-consult-region)
  (define-key map "\C-c\C-c" 'flora-consult-first)
  (define-key map "\C-c\C-a" 'flora-consult-file)
  (define-key map "\C-c\C-s" 'flora-reset-system)
  (define-key map "\C-c\C-i" 'flora-interrupt)
  (define-key map "\C-\\" 'flora-break)
)
(if flora-mode-map
    nil
  (setq flora-mode-map (make-sparse-keymap))
  (flora-mode-commands flora-mode-map))

;;;###autoload
(defun flora-mode ()
  "Major mode for editing F-Logic code.
Blank lines and `%%...' separate paragraphs.  `%'s start comments.
Commands:
\\{flora-mode-map}
Entry to this mode calls the value of `flora-mode-hook'
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map flora-mode-map)
  (setq major-mode 'flora-mode)
  (setq mode-name "Flora")
  (flora-mode-variables)
  (setq comint-prompt-regexp "^?-")
  (run-hooks 'flora-mode-hook))

(defun flora-indent-line (&optional whole-exp)
  "Indent current line as Flora code.
With argument, indent any additional lines of the same clause
rigidly along with this one (not yet)."
  (interactive "p")
  (let ((indent (flora-indent-level))
	(pos (- (point-max) (point))) beg)
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t")
    (if (zerop (- indent (current-column)))
	nil
      (delete-region beg (point))
      (indent-to indent))
    (if (> (- (point-max) pos) (point))
	(goto-char (- (point-max) pos)))
    ))


(defun flora-indentation-level-of-line ()
  "Return the indentation level of the current line."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (current-column)))


(defun flora-find-start-of-mline-comment ()
  "Return the start column of a /* */ comment.
This assumes that the point is inside a comment."
  (re-search-backward "/\\*" (point-min) t)
  (forward-char 2)
  (skip-chars-forward " \t")
  (current-column))

(defun flora-in-comment ()
  "Check if point is inside comment."
  ;; needs to be worked out
  nil)

(defun flora-indent-level ()
  "Compute Flora indentation level."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (cond
     ((looking-at "%%%") 0)		;Large comment starts
     ((looking-at "%[^%]") comment-column) ;Small comment starts
     ;;End of /* */ comment
     ((looking-at "\\*/")                  
      (save-excursion
	(flora-find-start-of-mline-comment)
	(skip-chars-backward " \t")
	(- (current-column) 2)))
     
     ;; Here we check if the current line is within a /* */ pair
     ((and (looking-at "[^%/]")
	   (flora-in-comment))
      (if flora-indent-mline-comments-flag
	  (flora-find-start-of-mline-comment)
	;; Same as before
	(flora-indentation-level-of-line)))
     ((bobp) 0)				;Beginning of buffer
     (t
      (let ((empty t) ind more less)
	(if (looking-at ")")
	    (setq less t)		;Find close
	  (setq less nil))
	;; See previous indentation
	(while empty
	  (forward-line -1)
	  (beginning-of-line)
 	  (if (bobp)
 	      (setq empty nil)
 	    (skip-chars-forward " \t")
 	    (if (not (or (looking-at "%[^%]") (looking-at "\n")))
 		(setq empty nil))))
 	(if (bobp)
 	    (setq ind 0)		;Beginning of buffer
	  (setq ind (current-column)))	;Beginning of clause
	;; See its beginning
	(if (looking-at "%%[^%]")
	    ind
	  ;; Real Prolog code
	  (if (looking-at "(")
	      (setq more t)		;Find open
	    (setq more nil))
	  ;; See its tail
	  (end-of-flora-clause)
	  (or (bobp) (forward-char -1))
	  (cond ((looking-at "[,(;>]")
		 (if (and more (looking-at "[^,]"))
		     (+ ind flora-indent-width) ;More indentation
		   (max tab-width ind))) ;Same indentation
		((looking-at "-") tab-width) ;TAB
		((or less (looking-at "[^.]"))
		 (max (- ind flora-indent-width) 0)) ;Less indentation
		(t 0))			;No indentation
	  )))
     )))

(defun end-of-flora-clause ()
  "Go to end of clause in this line."
  (beginning-of-line 1)
  (let* ((eolpos (save-excursion (end-of-line) (point))))
    (if (re-search-forward comment-start-skip eolpos 'move)
	(goto-char (match-beginning 0)))
    (skip-chars-backward " \t")))

(defun flora-comment-indent ()
  "Compute Prolog-style comment indentation."
  (cond ((looking-at "%%%") 0)
	((looking-at "%%") (flora-indent-level))
	((looking-at "//") (flora-indent-level))
	(t
	 (save-excursion
	       (skip-chars-backward " \t")
	       ;; Insert one space at least, except at left margin.
	       (max (+ (current-column) (if (bolp) 0 1))
		    comment-column)))
	))


;;;
;;; Inferior Flora mode
;;;
(defvar inferior-flora-mode-map nil)

(defun inferior-flora-mode ()
  "Major mode for interacting with an inferior Flora process.

The following commands are available:
\\{inferior-flora-mode-map}

Entry to this mode calls the value of `flora-mode-hook' with no arguments,
if that value is non-nil.  Likewise with the value of `comint-mode-hook'.
`flora-mode-hook' is called after `comint-mode-hook'.

You can send text to the inferior flora from other buffers
using the commands `process-send-region', `process-send-string' and \\[flora-consult-region].

Commands:
Tab indents for Flora; with argument, shifts rest
 of expression rigidly with the current line.
Paragraphs are separated only by blank lines and '%%'.
'%'s start comments.

Return at end of buffer sends line as input.
Return not at end copies rest of line to end and sends it.
\\[comint-kill-input] and \\[backward-kill-word] are kill commands, imitating normal Unix input editing.
\\[comint-interrupt-subjob] interrupts the shell or its current subjob if any.
\\[comint-stop-subjob] stops. \\[comint-quit-subjob] sends quit signal."
  (interactive)
  (require 'comint)
  (comint-mode)
  (setq major-mode 'inferior-flora-mode
	mode-name "Inferior-Flora"
	comint-prompt-regexp "^| [ ?][- ] *")
  (flora-mode-variables)
  (if inferior-flora-mode-map nil
    (setq inferior-flora-mode-map (copy-keymap comint-mode-map))
    (flora-mode-commands inferior-flora-mode-map))
  (use-local-map inferior-flora-mode-map)
  (run-hooks 'flora-mode-hook)
  (setq comint-input-ring-file-name (expand-file-name "~/.florid-history"))
  (comint-read-input-ring)
)

(defun run-flora-background ()
  "Run an inferior Flora process, input and output via buffer *flora*."
  (if (not (get-process "flora"))
      (save-excursion
	(set-buffer (make-comint "flora" flora-program-name))
	(inferior-flora-mode))))


;;;###autoload
(defun run-flora ()
  "Run an inferior Flora process, input and output via buffer *flora*, and
switch to the buffer."
  (interactive)
  (run-flora-background)
  (switch-to-buffer-other-window "*flora*")
)

(defun flora-consult-region (forget beg end)
  "Send the region to the Flora process.
The region must be made by \"M-x run-flora\" or created here.
If FORGET (prefix arg) is not nil, clear program before consulting. 
PROBLEM: the region has to be correct, complete input."
  (interactive "P\nr")
  (run-flora-background)
  (save-excursion
    (if forget
	(flora-reset-system))
    (process-send-string "flora" flora-consult-string)
    (process-send-region "flora" beg end)
    (process-send-string "flora" "\n")
    (process-send-eof "flora")) ;Send eof to flora process.
  (display-buffer "*flora*"))

(defun flora-consult-region-as-query (forget beg end)
  "Send the region to the Flora process as a query.
If FORGET (prefix arg) is not nil, clear program before consulting.
PROBLEM: every line of region has to be correct, complete input."
  (interactive "P\nr")
  (run-flora-background)
  (save-excursion
    (if forget
	(flora-reset-system))
    (process-send-region "flora" beg end)
    (process-send-string "flora" "\n"))		;May be unnecessary
  (display-buffer "*flora*"))

(defun flora-consult-buffer (forget)
  "Send the buffer region to the Flora process like \\[flora-consult-region]."
  (interactive "P")
  (flora-consult-region forget (point-min-marker) (point-max-marker))
)

(defun flora-interrupt()
  (interactive)
  (interrupt-process "flora"))

(defun flora-break()
  (interactive)
  (quit-process "flora"))

(defun flora-reset-system ()
  (interactive)
  (run-flora-background)
  (process-send-string "flora" flora-forget-string)
  )

(defun flora-switch-to-flora-buffer ()
  (interactive)
  (run-flora-background)
  (pop-to-buffer "*flora*"))

(defun flora-consult-first ()
  "Reset system before consulting buffer as file"
  (interactive)
  (flora-consult-file t))

(defun flora-consult-file (dynamically)
  "Prompt to save all buffers and run Flora on current buffer's file.
If DYNAMICALLY (prefix arg) is not nil, consult into dynamic area.
This function is more useful than \\[flora-consult-buffer]."
  (interactive "P")
  (if (not (buffer-file-name))
      (error "Buffer does not seem to be associated with any file"))
  (if flora-offer-save
      (save-some-buffers))
  (run-flora-background)
  (process-send-string "flora"
		       (if dynamically
			   (format "<%s>.\n" buffer-file-name)
			 (format "[%s].\n" buffer-file-name)))
  (display-buffer "*flora*")
)

;;; flora.el ends here


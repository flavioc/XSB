;;; flora.el --- a major mode for editing and running F-Logic programs

;; Authors:
;; Heinz Uphoff (uphoff@informatik.uni-freiburg.de)
;; Christian Schlepphorst (schlepph@informatik.uni-freiburg.de)
;; Michael Kifer (kifer@cs.sunysb.edu)

;;; Commentary:

;; This package provides a major mode for editing F-Logic.  It knows
;; about Flora syntax and comments (well, sort of), and can send
;; regions, buffers, and files to an inferior F-Logic interpreter
;; process.

;; This package was adapted from flp.el by Michael Kifer.
;; In turn, flp.el, a major mode for FLORID, was from prolog.el by
;; Heinz Uphoff and Christian Schlepphorst.

;;
;; Put
;;   (setq auto-mode-alist (cons '("\\.flr$" . flora-mode) auto-mode-alist))
;;   (autoload 'flora-mode "flora" "Major mode for editing Flora programs." t)

;;; Code:

(require 'comint)

;; Is it XEmacs?
(defconst flora-xemacs-p (string-match "XEmacs" emacs-version))
;; Is it Emacs?
(defconst flora-emacs-p (not flora-xemacs-p))

(defconst flora-temp-file-prefix
  (cond (flora-emacs-p temporary-file-directory)
	((fboundp 'temp-directory) (temp-directory))
	(t "/tmp/"))
  "*Directory for temporary files.")

;; This path has to be set at installation of the F-Logic-System!!!
(defvar flora-program-name "xsb"
  "*Program name for invoking an inferior Flora with `run-flora'.")

(defvar flora-mode-syntax-table nil)
(defvar flora-mode-abbrev-table nil)
(defvar flora-mode-map nil)

(defvar flora-forget-string "halt.\n\n"
  "*Reinitialise  system")

(defconst flora-process-buffer "*flora*"
  "Name of the Flora buffer.")
(defconst flora-process-name "flora"
  "Name of Flora process.")

(defvar flora-offer-save t
  "*If non-nil, ask about saving modified buffers before 
\\[flora-consult-file] is run.")

(defvar flora-electric nil
  "*If t, typing RETURN automatically indents Flora lines.")

(defvar flora-indent-width 4)

(defvar flora-indent-mline-comments-flag t
  "*Non-nil means automatically align comments when indenting.")

(defconst flora-quoted-atom-regexp
  "\\(^\\|[^0-9]\\)\\('\\([^\n']\\|\\\\'\\)*'\\)"
  "Regexp matching a quoted atom.")
(defconst flora-atom-regexp
  "\\(%s\\|[A-Za-z0-9_]+\\)"
  "Regexp matching an atom.")
(defconst flora-string-regexp
  "\\(\"\\([^\n\"]\\|\\\\\"\\)*\"\\|'\\([^\n']\\|''\\)*'\\)"
  "Regexp matching a string.")
(defconst flora-bracketed-object "\\[.*\\]"
  "Like list. Used to prevent recursion in flora-list-regexp.")
(defconst flora-list-regexp
  (format "\\[\\( %s \\| %s \\)\\]" flora-atom-regexp flora-bracketed-object)
  "Regexp for matching a list.")
(defconst flora-oid-regexp
  (format "\\(%s\\|%s\\|%s\\|%s\\|[A-Za-z0-9]+\\)"
	  flora-list-regexp flora-bracketed-object
	  flora-string-regexp flora-atom-regexp)
  "Regexp to recognize atoms.")

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
    '("\\([A-Za-z0-9_][A-Za-z0-9_!.]*\\) *\\["
      1 'font-lock-variable-name-face)
    '("\\b\\(not\\|avg\\|sum\\|count\\|collectset\\|collectbag\\|assert\\|retract\\|erase\\|retractall\\)\\b"
      1 'font-lock-keyword-face)
    '("\\(:\\|;\\)" 
      1 'font-lock-type-face)
    '("\\(\\[\\|\\]\\|{\\|}\\)"
      1 'bold)
    '("\\b\\(index\\|from\\|table\\|import\\|export\\)\\b"
      1 'flora-font-lock-system-face)
    '("\\(\\b[A-Za-z0-9_]+\\b *\\((\\b[^)]+\\b)\\)?\\)[ \t\n]*\\(@[ \t\n]*(.*)[ \t\C-m]*\\)?\\*?[---=]>"
      1 'font-lock-function-name-face)
    )
  "Additional expressions to highlight in flora mode.")

(defvar flora-mode-menu
  '(["Consult file"    flora-consult-file   t]
    ["Consult buffer"  flora-consult-buffer t]
    ["Consult region"  flora-consult-region t]
    "---"
    ["Consult file dynamically"    flora-consult-file-dynamically   t]
    ["Consult buffer dynamically"  flora-consult-buffer-dynamically t]
    ["Consult region dynamically"  flora-consult-region-dynamically t]
    "---"
    ["Start Flora process"     run-flora    	    t]
    ["Restart Flora process"   flora-restart	    t]
    "---"
    ["Interrupt Flora process" flora-interrupt	    t]
    ["Quit Flora process"      flora-quit    	    t]
    ))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if flora-mode-syntax-table
    ()
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_    "_"      table)
    (modify-syntax-entry ?\\   "\\"     table)
    (modify-syntax-entry ?+    "."      table)
    (modify-syntax-entry ?-    "."      table)
    (modify-syntax-entry ?=    "."      table)
    (modify-syntax-entry ?%    "<"  table)
    (modify-syntax-entry ?\n   ">"    table)
    (modify-syntax-entry ?\C-m ">"    table)
    (modify-syntax-entry ?<    "."      table)
    (modify-syntax-entry ?>    "."      table)
    (modify-syntax-entry ?\'   "\""     table)
    ;; the // comment style isn't supported, due to the limitation of emacs
    (modify-syntax-entry ?/    ". 14b" table)
    (modify-syntax-entry ?*    ". 23b"   table)
    (setq flora-mode-syntax-table table)
    ))


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
  (define-key map "\C-c\C-r" 'flora-consult-region)
  (define-key map "\C-c\C-f" 'flora-consult-file)
  (define-key map "\C-c\C-s" 'flora-restart)
  (define-key map "\M-\t"    'comint-dynamic-complete)
  (define-key map "\C-c\C-c" 'flora-interrupt)
  (define-key map "\C-c\C-d" 'flora-quit)
  (define-key map "\C-m"     'flora-electric-return)
  (define-key map "[return]" 'flora-electric-return)
  (define-key map "*"	     'flora-electric-star)
  (define-key map "/"	     'flora-electric-slash))


;; Set up Flora keymap
(if flora-mode-map
    nil
  (setq flora-mode-map (make-sparse-keymap))
  (flora-mode-commands flora-mode-map))

;; Set up Flora menus
(if window-system
    (easy-menu-define flora-menubar flora-mode-map "Flora Commands"
		      (cons "Flora" flora-mode-menu))
  )

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
  (if (re-search-backward "/\\*" (point-min) t)
      (progn
	(forward-char 2)
	(skip-chars-forward " \t")
	(current-column))
    (error "Not inside a comment")
    ))

(defun flora-in-mline-comment ()
  "Check if point is inside comment."
  (let ((pt (point)))
    (save-excursion
      (if (re-search-backward "/\\*" (point-min) t)
	  ;; If after searching backward and finding /* we search forward
	  ;; and find */ then we aren't in comment
	  (not (re-search-forward "\\*/" pt t))
	))
    ))

(defun flora-indent-level ()
  "Compute Flora indentation level."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (cond
     ((looking-at "%%%") 0)		;Large comment starts
     ((and (looking-at "%[^%]")
	   (not (flora-in-mline-comment)))
      comment-column) ;Small comment starts
     ;;End of /* */ comment
     ((or (looking-at "\\*/") (looking-at "\\*\\*"))
      (save-excursion
	(flora-find-start-of-mline-comment)
	(skip-chars-backward " \t")
	(- (current-column) 2)))
     
     ;; Here we check if the current line is within a /* */ pair
     ((and (looking-at "[^\\*/]")
	   (flora-in-mline-comment))
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

(defun flora-electric-star (arg)
  "Insert a star character.
If the star is the second character of a C style comment introducing
construct, and we are on a comment-only-line, indent line as comment.
If numeric ARG is supplied or point is inside a literal, indentation
is inhibited."
  (interactive "*P")
  ;; if we are not in a comment, or if arg is given do not re-indent the
  ;; current line, unless this star introduces a comment-only line.
  (let ((indentp (and (not arg)
		     (flora-in-mline-comment)
		     (eq (char-before) ?*)
		     (save-excursion
		       (forward-char -1)
		       (skip-chars-backward "*")
		       (if (eq (char-before) ?/)
			   (forward-char -1))
		       (skip-chars-backward " \t")
		       (bolp)))))
    (self-insert-command (prefix-numeric-value arg))
    (if indentp
	(flora-indent-line))
    ))

(defun flora-electric-slash (arg)
  "Insert a slash character.

Indent the line as a comment, if:
The slash is part of a `*/' token that closes a block oriented comment.

If numeric ARG is supplied or point is inside a literal, indentation
is inhibited."
  (interactive "*P")
  (let* ((ch (char-before))
	 (indentp (and (not arg)
		       (eq last-command-char ?/)
		       (or (and (eq ch ?/)
				(not (flora-in-literal)))
			   (and (eq ch ?*)
				(flora-in-mline-comment)))
		       ))
	 )
    (self-insert-command (prefix-numeric-value arg))
    (if indentp
	(flora-indent-line))))

(defun flora-electric-return ()
  "If flora-electric is t, indent automatically whenever the user types RETURN.
Otherwise, just insert newline."
  (interactive)
  (insert "\n")
  (if flora-electric
      (indent-according-to-mode)))

(defun flora-in-literal ()
  ;; to be worked out
  nil)



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

Entry to this mode calls the value of `inferior-flora-mode-hook' with no
arguments, if that value is non-nil.  Likewise with the value of
`comint-mode-hook'. 
`inferior-flora-mode-hook' is called after `comint-mode-hook'.

You can send text to the inferior flora from other buffers
using the commands \\[flora-consult-buffer] \\[flora-consult-file], and
\\[flora-consult-region]. 

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
	comint-prompt-regexp "flora *[ ?][- ] *")
  (flora-mode-variables)
  (if inferior-flora-mode-map nil
    (setq inferior-flora-mode-map (copy-keymap comint-mode-map))
    (flora-mode-commands inferior-flora-mode-map))
  (use-local-map inferior-flora-mode-map)
  (run-hooks 'inferior-flora-mode-hook)
  (setq comint-input-ring-file-name 
	(expand-file-name "~/.flora-history"))
  (or (file-exists-p comint-input-ring-file-name)
      (write-region 1 1 comint-input-ring-file-name))
  (comint-read-input-ring)
)

(defun run-flora-background ()
  "Run an inferior Flora process, input and output via buffer *flora*."
  (if (not (get-process flora-process-name))
      (save-excursion
	(set-buffer
	 (make-comint flora-process-name
		      flora-program-name
		      nil
		      "-e"
		      "[flora], flora_shell."))
	(inferior-flora-mode))))


;;;###autoload
(defun run-flora ()
  "Run an inferior Flora process, input and output via buffer *flora*, and
switch to the buffer."
  (interactive)
  (run-flora-background)
  (show-flora-buffer 'switch))

(defun flora-consult-region (dynamically beg end)
  "Send the region to the Flora process.
The region must be made by \"M-x run-flora\" or created here.
If DYNAMICALLY (prefix arg) is not nil, consult dynamically."
  (interactive "P\nr")
  (let ((tmpfile-name (flora-make-temp-file beg end)))
    (run-flora-background)
    (save-excursion
      (process-send-string flora-process-name 
			   (format (if dynamically
				       "<'%s'>.\n"
				     "['%s'].\n")
				   tmpfile-name)))
    (show-flora-buffer)
    ))

(defun flora-consult-region-as-query (beg end)
  "Send the region to the Flora process as a query.
The region must be a valid query terminated with a period."
  (interactive "P\nr")
  (run-flora-background)
  (save-excursion
    (process-send-region flora-process-name beg end)
    (process-send-string flora-process-name "\n"))
  (show-flora-buffer))

(defun flora-consult-buffer (dynamically)
  "Send the current buffer to the Flora process.
Do not offer to save files.
If buffer is associated with a file, then the buffer is saved in a temporary
file with the same name, so that the consulted code will have the same module
name as the original file. If the buffer is not associated with any file, a
temporary file with a random name is used."
  (interactive "P")
  (let ((file (file-name-nondirectory (buffer-file-name)))
	flora-offer-save)
    (if file
	(progn
	  (setq file (concat flora-temp-file-prefix file))
	  (write-region (point-min) (point-max) file)
	  (flora-consult-file dynamically file))
      (flora-consult-region 
       (if dynamically
	   t nil)
       (point-min-marker) (point-max-marker)))
    ))


(defun flora-consult-file (dynamically &optional file)
  "Prompt for a file, offer to save all buffers, then run Flora on the file.
If DYNAMICALLY (prefix arg) is not nil, consult into dynamic area."
  (interactive "P")
  (let ((default-file (if (buffer-file-name)
			  (buffer-file-name)
			"none")))
    (if (not (stringp file))
	(setq file
	      (read-file-name
	       (format "File name to consult %s(%s): "
		       (if dynamically "dynamically " "")
		       (file-name-nondirectory default-file))
	       nil default-file)))
    (if flora-offer-save
	(save-some-buffers))
    (run-flora-background)
    (process-send-string flora-process-name
			 (if dynamically
			     (format "<'%s'>.\n" file)
			   (format "['%s'].\n" file)))
    (show-flora-buffer)))

(defun flora-consult-file-dynamically ()
  (interactive)
  (flora-consult-file t))

(defun flora-consult-buffer-dynamically ()
  (interactive)
  (flora-consult-buffer t))

(defun flora-consult-region-dynamically (beg end)
  (interactive "r")
  (flora-consult-region t beg end))

(defun flora-interrupt()
  (interactive)
  (interrupt-process flora-process-name))

(defun flora-quit()
  (interactive)
  (quit-process flora-process-name))

(defun flora-restart ()
  (interactive)
  (run-flora-background)
  (process-send-string flora-process-name flora-forget-string)
  (sit-for 2)
  (run-flora)
  (sit-for 0))

(defun flora-switch-to-flora-buffer ()
  (interactive)
  (run-flora-background)
  (pop-to-buffer flora-process-buffer))

;; SWITCH means switch to inferior Flora buffer
(defun show-flora-buffer (&optional switch)
  (let ((wind (selected-window)))
    (with-temp-buffer
      (set-buffer flora-process-buffer)
      (display-buffer flora-process-buffer)
      (switch-to-buffer-other-window flora-process-buffer)
      ;; time is needed for XSB to return. otherwise, the point will be off
      (sit-for 1))
      (goto-char (1- (point-max)))
      (or switch
	  (select-window wind))))


(defun flora-make-temp-file (start end)
  (let* ((f (make-temp-name (concat flora-temp-file-prefix "flora"))))
    
    ;; create the file
    (write-region start end
		  (concat f ".flr")
		  nil          ; don't append---erase
		  'no-message) 
    (expand-file-name f)))

;;; flora.el ends here

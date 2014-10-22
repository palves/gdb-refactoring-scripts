;;; This rewrites TRY_CATCH into TRY+CATCH+END_CATCH

(defconst dir-to-scan (pop argv))
(unless (file-directory-p dir-to-scan)
  (error "Usage: emacs --script tc-rewrite.el DIR"))

(defconst trycatch-dirs '("." "cli" "mi" "tui" "nat" "common" "python" "guile" "gdbserver"))

(defun trycatch-files ()
  (apply #'nconc
	 (mapcar (lambda (dir)
		   (directory-files (expand-file-name dir dir-to-scan)
				    t "\\.c$"))
		 trycatch-dirs)))

(defconst trycatch-symbol-rx
  ;; bizarrely we can't seem to do this with [:something:].
  "\\_<[[:alnum:]_]+\\_>")

(defconst trycatch-tc-rx
  (concat "TRY_CATCH\s-*(\\s-*\\(" trycatch-symbol-rx "\\)\\s-*"
	  ",\\s-*RETURN_MASK_\\([^)]+\\))"))

(defconst trycatch-decl-rx
  "^\\s-*volatile struct gdb_exception\\s-*[^\n]*\n")

(defconst trycatch-cont-break-rx
  "\\_<\\(break\\|continue\\)\\_>")

;; check if conforming and return end boundary if so.
(defun trycatch-conforming ()
  (skip-chars-forward " \t\n")
  (if (looking-at "{")
      (save-excursion (forward-sexp) (point))
    (message "%s:%d: non-conforming TRY_CATCH"
	     (buffer-file-name)
	     (line-number-at-pos))
    nil))

(defun trycatch-warn-cb (limit)
  (while (re-search-forward trycatch-cont-break-rx limit t)
    (message "%s:%d: %s in TRY_CATCH"
	     (buffer-file-name)
	     (line-number-at-pos)
	     (match-string 0))))

;; This function was going in the direction of converting to C++
;; try/catch directly.  Currently unused.

;;; This rewrites TRY_CATCH to "try" and sometimes an
;;; additional "catch".  It isn't quite finished.
;;; The output isn't valid, it still needs post-editing

;;; TO DO:
;;; * RETURN_MASK handling must be implemented
;;;   it should turn into a different type on the 'catch'
;;; * unconditionally emit a "catch", sometimes "catch (...) { }"

(defun trycatch-rewrite-try ()
  (goto-char (point-min))
  (while (re-search-forward trycatch-tc-rx nil t)
    (let ((symbol (match-string 1))
	  (mask (match-string 2)))
      (replace-match "try")
      (let ((end-of-body (trycatch-conforming)))
	(when end-of-body
	  (trycatch-warn-cb end-of-body)
	  (goto-char end-of-body)
	  (skip-chars-forward " \t\n")
	  (if (looking-at (concat "if (" symbol "\\.reason < 0)"))
	      (replace-match (concat "catch (const gdb_exception &"
				     symbol ")"))))))))

;; Start TRY_CATCH -> TRY+CATCH+END_CATCH conversion.

;; Remove all local volatile struct gdb_exception objects.  They
;; aren't necessary in the new scheme.

(defun trycatch-zap-exceptions ()
  (goto-char (point-min))
  (while (re-search-forward trycatch-decl-rx nil t)
    (replace-match "")))

;; Add braces around statement, and indent region.
(defun trycatch-add-block-braces (here type)
  (when (not (looking-at "{"))
    (message "%s:%d: %s block with no start brace: fixing"
	     file (line-number-at-pos here) type)
    (let ((here (point)))
      (insert "{")
      (newline)
      (re-search-forward ";" nil 'move)
      (newline)
      (insert "}")
      (indent-region here (point))
      )
    )
  )

;; Fix block TYPE, adding braces.

(defun trycatch-fix-block (type)
  (goto-char (point-min))
  (while (re-search-forward (concat "^[^#].*\\_<" type "\\_>") nil 'move)
    (save-excursion
      (skip-chars-forward " \t\n")
      (let ((here (point)))
	(when (looking-at "(")
	  (forward-sexp)
	  (skip-chars-forward " \t\n")
	  (trycatch-add-block-braces here type))))))

;; Fix up non-braced TRY_CATCH bodies, adding braces.

(defun trycatch-fix-try-block ()
  (trycatch-fix-block "TRY_CATCH"))

;; Fix up non-braced CATCH bodies, adding braces.

(defun trycatch-fix-catch-block ()
  (trycatch-fix-block "CATCH"))

;; Zap TRY_CATCH (...) -> TRY

(defun trycatch-zap-try-catch-head (here)
  (goto-char here)
  (re-search-backward "_CATCH" nil 'move)
  (re-search-forward "_CATCH.*)" nil 'move)
  (replace-match ""))

(defun trycatch-find-close-brace ()
  (save-excursion
    (if (re-search-forward "}" nil 'move)
	(point)
      nil)))

;; Convert TRY_CATCH into TRY + CATCH.  Adding END_CATCH is done as a
;; separate step, as other passes also add CATCH blocks.

(defun trycatch-split-try-and-catch ()
  (goto-char (point-min))
  ;; Skip #define TRY_CATCH ..
  (while (re-search-forward "\\_<TRY_CATCH\\_>" nil 'move)
    (save-excursion
      (skip-chars-forward " \t\n")
      (let ((here (point)))
	(if (not (looking-at "("))
	    ;; Probably a comment -- skip it.
	    nil
	  (forward-sexp)
	  (let ((trycatch_end (point)))
	    (skip-chars-forward " \t\n")
	    ;; We should have fixed all blocks to have braces by now.
	    (if (not (looking-at "{"))
		(message "%s:%d: error: TRY_CATCH with no start brace?"
			 file
			 (line-number-at-pos here))
	      (forward-sexp)
	      (let ((closebrace (trycatch-find-close-brace)))
		(if (not closebrace)
		    (message "%s:%d: error: TRY_CATCH with no end brace?"
			     file
			     (line-number-at-pos here))
		  (when (re-search-forward "if .*reason < 0)" closebrace 'move)
		    (replace-match (concat "CATCH " (buffer-substring here trycatch_end)))
		    (indent-for-tab-command)
		    (trycatch-zap-try-catch-head here)
		    ))))))))))

(defun trycatch-python-scheme ()
  (goto-char (point-min))
  ;; Skip #define TRY_CATCH ..
  (while (re-search-forward "\\_<TRY_CATCH\\_>" nil 'move)
    (save-excursion
      (skip-chars-forward " \t\n")
      (let ((here (point)))
	(if (not (looking-at "("))
	    ;; Probably a comment -- skip it.
	    nil
	  (forward-sexp)
	  (let ((trycatch_end (point)))
	    (skip-chars-forward " \t\n")
	    ;; We should have fixed all blocks to have braces by now.
	    (if (not (looking-at "{"))
		(message "%s:%d: error: TRY_CATCH with no start brace?"
			 file
			 (line-number-at-pos here))
	      (forward-sexp)
	      (skip-chars-forward " \t\n")
	      (when (looking-at "\\(GDB_PY_.*_EXCEPTION\\|GDBSCM_HANDLE_GDB_EXCEPTION\\)[^\r\n]*;")
		(let ((catch-block (point)))
		  (insert (concat "CATCH " (buffer-substring here trycatch_end)))
		  (newline)
		  (insert "{")
		  (newline)
		  (re-search-forward ";" nil 'move)
		  (skip-chars-forward " \t")
		  (newline)
		  (insert "}")
		  (indent-region catch-block (point))

		(trycatch-zap-try-catch-head here))))))))))

;; Check if we have an END_CATCH block already and return its location
;; if so.
(defun trycatch-next-symbol-is (symbol)
  (save-excursion
    (skip-chars-forward " \t\n")
    (if (looking-at symbol)
	(point)
      nil)))

;; Go over remaining TRY_CATCH blocks and append an empty CATCH block.

(defun trycatch-add-catch-block ()
  (goto-char (point-min))
  ;; Skip #define TRY_CATCH ..
  (while (re-search-forward "^[^#].*\\_<TRY_CATCH\\_>" nil 'move)
    (save-excursion
      (skip-chars-forward " \t\n")
      (let ((here (point)))
	(if (not (looking-at "("))
	    ;; Probably a comment -- skip it.
	    nil
	  (forward-sexp)
	  (let ((trycatch_end (point)))
	    (skip-chars-forward " \t\n")
	    ;; It is hard to handle non-braced bodies in elisp.
	    (if (not (looking-at "{"))
		(message "%s:%d: non-conforming TRY_CATCH (no start brace)"
			 file
			 (line-number-at-pos here))
	      (forward-sexp)
	      (let ((insert_point (point)))
		(when (not (trycatch-next-symbol-is "CATCH"))
		  (newline)
		  (insert (concat "CATCH " (buffer-substring here trycatch_end)))
		  (newline)
		  (insert "{")
		  (newline)
		  (insert "}")
		  (indent-region insert_point (point))

		  (trycatch-zap-try-catch-head here))))))))))

;; Go over all CATCH blocks and append an END_CATCH.

(defun trycatch-end-catch ()
  (goto-char (point-min))
  (while (re-search-forward "^[^#].*\\_<CATCH\\_>" nil 'move)
    (save-excursion
      (skip-chars-forward " \t\n")
      (let ((here (point)))
	(if (not (looking-at "("))
	    ;; Probably a comment -- skip it.
	    nil
	  (forward-sexp)
	  (skip-chars-forward " \t\n")
	  ;; It is hard to handle non-braced bodies in elisp.
	  (if (not (looking-at "{"))
	      (message "%s:%d: non-conforming CATCH block (no start brace)"
		       file
		       (line-number-at-pos here))
	    (forward-sexp)
	    (when (not (trycatch-next-symbol-is "END_CATCH"))
	      (newline-and-indent)
	      (insert "END_CATCH")

	      ;; Add a newline if there isn't one yet, and if not
	      ;; closing a block.
	      (forward-char)
	      (when (not (looking-at "\n"))
		(skip-chars-forward " \t")
		(when (not (looking-at "}"))
		  (move-beginning-of-line 1)
		  (newline))))))))))

(defun trycatch-rewrite-one (file)
  (find-file file)
  (trycatch-zap-exceptions)
  (trycatch-fix-try-block)
  (trycatch-split-try-and-catch)
  (trycatch-python-scheme)
  (trycatch-add-catch-block)
  (trycatch-fix-catch-block)
  (trycatch-end-catch)
  (if (buffer-modified-p)
      (save-buffer)))

(defun trycatch-rewrite ()
  (dolist (file (trycatch-files))
    (message "Processing %s" file)
    (trycatch-rewrite-one file)))

(trycatch-rewrite)

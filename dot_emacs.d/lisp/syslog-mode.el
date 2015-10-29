(defvar syslog-mode-processes '(
				"session_ctrl"
				"gtpc_sig"
				"dia_client"
				"pmip_sig"
				"GWUP"
				"gwup_proxy"
				"lib_client"
                "sys_ctrl"
                "trace_ctrl"
                "conf_proxy"
				)
  )


(defvar syslog-mode-process-regexp (regexp-opt syslog-mode-processes 'words))

(setq syslog-mode-keywords
      `(
	;;errors warns and crits
	("^[^ ]+ [^ ]+ [^ ]+ \\(err\\|crit\\|warn\\)[^\n]+" . font-lock-warning-face)
	;;as node name
	("\\(AS\\|SAB\\|SE\\)[0-9]+-[0-9]+" . font-lock-keyword-face)
	;;internal messages
	("INT_MSG_[^:]+: [^ ]+" . font-lock-constant-face)
	;;external messages
	("EXT_MSG_[^:]+: [^ ]+ [^ ]+" . font-lock-constant-face)
	;;process name
	(,syslog-mode-process-regexp . font-lock-function-name-face)
	)
      )

(defun syslog-mode-logging-process-at-point()
  (save-excursion
    (goto-char (line-beginning-position))
    (looking-at "^[^ ]+ +[^ ]+ +[^ ]+ +[^ ]+ +[^ ]+ \\([a-zA-Z_]+\\)")
    (buffer-substring-no-properties (match-beginning 1) (match-end 1))
    )
  )

(defvar syslog-mode-process-match  "^[^ ]+ +[^ ]+ +[^ ]+ +[^ ]+ +[^ ]+ \\(%s\\)")
(defun syslog-mode-end-of-transaction()
  (interactive)
  (let* ((process (syslog-mode-logging-process-at-point))
	 (matcher (format syslog-mode-process-match process))
	 (transaction-end-regexp (format "%s.* --------- TRANSACTION END" matcher))
	 )
    (re-search-forward transaction-end-regexp nil t)
    (goto-char (+ (line-end-position) 1))
    )
  )

(defun syslog-mode-beginning-of-transaction()
  (interactive)
  (let* ((process (syslog-mode-logging-process-at-point))
	 (matcher (format syslog-mode-process-match process))
	 (transaction-begin-regexp (format "%s.* --------- TRANSACTION [^E]" matcher))
	 )
    (re-search-backward transaction-begin-regexp nil t)
    (goto-char (line-beginning-position))
    )
  )

(defun syslog-mode-gather-transaction()
  "Gather the logs of belonging to the transaction under point together.

"
  (interactive)
  (goto-char (line-end-position))
  (let* ((process (syslog-mode-logging-process-at-point))
	 (current-line (line-beginning-position))
	 (matcher (format syslog-mode-process-match process))
	 (transaction-end-regexp (format "%s.* --------- TRANSACTION END" matcher))
	 (transaction-begin (re-search-backward 
			     (format "%s.* --------- TRANSACTION " matcher)))
	 (done nil)
	 (transaction nil)
	 )
    (while (not done)
      (goto-char (line-beginning-position))
      (if (looking-at transaction-end-regexp) (set 'done t))
      (if (not (looking-at matcher)) (if (= (forward-line) 1) (set 'done t))
	(push (buffer-substring (line-beginning-position) (+ (line-end-position) 1))
	      transaction)
	(delete-region (line-beginning-position) (+ (line-end-position) 1))
	)
      )
    (goto-char transaction-begin)
    (mapc (lambda(l) (insert l)) (nreverse transaction))
    )
  )

(defun syslog-mode-show-process(process)
  "Show the possibly hidden process."
  (interactive "MShow process: ")
  (save-excursion
    (goto-char (point-min))
    (while (progn 
	     (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
		    )
	       (if (string-match (format "[^ ]+ +[^ ]+ +[^ ]+ +[^ ]+ +[^ ]+ %s" process) line)
		   (remove-text-properties (line-beginning-position) (+ (line-end-position) 1) '(invisible))
		 )
	       )
	     (/= (forward-line) 1)
	     )
      )
    )
  )

(defun syslog-mode-hide-process(process)
  "Hide all logs from the process."
  (interactive "MHide process (default process at line): ")
  (if (string= "" process) 
      (save-excursion
	(goto-char (line-beginning-position))
	(looking-at "^[^ ]+ +[^ ]+ +[^ ]+ +[^ ]+ +[^ ]+ \\([a-zA-Z_]+\\)")
	(set 'process (buffer-substring-no-properties (match-beginning 1) (match-end 1))) 
	)
    )
  (message "hiding %s" process)  
  (save-excursion
    (goto-char (point-min))
    (while (progn 
	     (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
		    )
	       (if (string-match (format "[^ ]+ +[^ ]+ +[^ ]+ +[^ ]+ +[^ ]+ %s" process) line)
		   (put-text-property (line-beginning-position) (+ (line-end-position) 1) 'invisible t)
		 )
	       )
	     (/= (forward-line) 1)
	     )
      )
    )
  )

(defun syslog-mode-other-end-of-this()
  (interactive)
  (let* ((sym (thing-at-point 'symbol))
	 (direction (if (string-match "INT_MSG_OUT" (thing-at-point 'line)) "INT_MSG_IN"
		      "INT_MSG_OUT"
		      ))
	 (pattern (format "%s: %s" direction sym))
	 (after (save-excursion (re-search-forward pattern nil t)))
	 (before (save-excursion (re-search-backward pattern nil t)))
	 (found-point (cond ((not after) before)
			    ((not before) after)
			    ((<= (- after (point)) (- (point) after)) after)
			    (t before))
		      )
	 )
    (if found-point (goto-char found-point)
      (message "did not foind the other end of %s" sym)
      )
    )
  )

(define-derived-mode syslog-mode fundamental-mode
  "syslog mode"
  "Major mode for analyzing ATCA syslogs.

Defined keys:
\\{syslog-mode-map}

Pressing \\[mark-defun] will select the transaction the point is at, \\[end-of-defun] will move to the end
of the transaction and \\[beginning-of-defun] will move to the start of the transaction. 
"
  (set (make-local-variable 'font-lock-defaults) '(syslog-mode-keywords))
  (define-key syslog-mode-map "\M-." 'syslog-mode-other-end-of-this)
  (define-key syslog-mode-map "\C-ch" 'syslog-mode-hide-process)
  (define-key syslog-mode-map "\C-cs" 'syslog-mode-show-process)
  (define-key syslog-mode-map "\C-cg" 'syslog-mode-gather-transaction)
  (set (make-local-variable 'end-of-defun-function) 'syslog-mode-end-of-transaction)
  (set (make-local-variable 'beginning-of-defun-function) 'syslog-mode-beginning-of-transaction)
  )

;;automode-alist
(setq auto-mode-alist
      (append
       '(("\\.top\\'" . topdrawer-mode)
	 ("\\.tdr\\'" . topdrawer-mode)
         ("\\.td\\'"  . topdrawer-mode))
       auto-mode-alist))
;;Define Topdrawer Options 
(defvar topdrawer-option-xwindow "-d xw")
(defvar topdrawer-option-ps  "-d postscr,ddname=")
(defvar topdrawer-option-eps  "-d eps,ddname=")
(defvar topdrawer-ugsoption nil)
(defvar topdrawer-option "-w")
(defvar topdrawer-extra-option "-b")
(defvar topdrawer-command "td")

;;Define Topdrawer-mode syntax-table
(defvar topdrawer-mode-syntax-table nil)
(if topdrawer-mode-syntax-table
    ()
  (setq topdrawer-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?< "(> " topdrawer-mode-syntax-table)
  (modify-syntax-entry ?> ")< " topdrawer-mode-syntax-table))
  
;;Define Topdrawer-Mode Keymap
(defvar topdrawer-mode-map nil)
(setq topdrawer-mode-map (make-sparse-keymap))
(define-key topdrawer-mode-map [menu-bar topdrawer]
  (cons "Topdrawer" (make-sparse-keymap "Topdrawer")))
(define-key topdrawer-mode-map [menu-bar topdrawer eps]
  '("Topdrawer EPS" . topdrawer-eps))
(define-key topdrawer-mode-map [menu-bar topdrawer postscript]
  '("Topdrawer PS" . topdrawer-ps))
(define-key topdrawer-mode-map [menu-bar topdrawer quit]
  '("Topdrawer Exit" . exit-topdrawer-process))
(define-key topdrawer-mode-map [menu-bar topdrawer process]
  '("Topdrawer Next" . continue-topdrawer-process))
(define-key topdrawer-mode-map [menu-bar topdrawer xwindow]
  '("Topdrawer Xwindow" . topdrawer-xwindow))
(define-key topdrawer-mode-map "\C-t\C-d" 'topdrawer-xwindow)
(define-key topdrawer-mode-map "\C-t\C-f" 'continue-topdrawer-process)
(define-key topdrawer-mode-map "\C-t\C-q" 'exit-topdrawer-process)
(define-key topdrawer-mode-map "\C-t\C-p" 'topdrawer-ps)
(define-key topdrawer-mode-map "\C-t\C-e" 'topdrawer-eps)

;;Topdrawer-mode
(defun topdrawer-mode ()
  (interactive)
  (setq mode-name "Topdrawer")
  (setq major-mode 'topdrawer-mode)
  (use-local-map topdrawer-mode-map)
  (set-syntax-table topdrawer-mode-syntax-table)
  (run-hooks 'text-mode-hook 'topdrawer-mode-hook))

;;
(defun topdrawer-xwindow ()
  "Topdrawer-Xwindow"
  (interactive)
  (if (not (string= (process-status "topdrawer") nil))
      (delete-process "topdrawer")
    )
  (setq start (point-min) end (point-max))
  (td-exec start end topdrawer-option-xwindow)
)

(defun topdrawer-ps ()
  "Topdrawer-Postscript"
  (interactive)
  (if (not (string= (process-status "topdrawer") nil))
      (delete-process "topdrawer")
    )
  (setq f1 nil)
  (setq f2 nil)
  (setq filename nil)
  (setq f1 (buffer-file-name (current-buffer)))
  (if (string= f1 nil)
      (setq f3  f1)
      (setq f3 (file-name-nondirectory f1))
      )
  (if (string= f3 nil)
      (setq filename (read-file-name "PSname:"))
      (if (or
	    (string= (substring f3 -3 nil) "top")
	    (string= (substring f3 -3 nil) "tdr"))
          (setq f2 (concat (substring f3 0 -4) ".ps"))
          (setq f2 (concat f3 ".ps")))
      (let ((insert-default-directory t))
       (setq filename (read-file-name "PSname:" nil f2 nil f2))
       )
      )
  (setq filename (expand-file-name filename))
;  (if (string= (file-name-directory f1) (file-name-directory filename))
;	   (setq filename (file-name-nondirectory filename) )
;	 )
 
  (message "Writting to %s" filename)
  (setq filename (concat "\"" filename "\""))
  (setq start (point-min) end (point-max))
  (setq tdoption-ps (concat topdrawer-option-ps filename))
  (td-exec-ps start end tdoption-ps)
)

(defun topdrawer-eps ()
  "Topdrawer-EnhancedPostscript"
  (interactive)
  (if (not (string= (process-status "topdrawer") nil))
      (delete-process "topdrawer")
    )
  (setq f1 nil)
  (setq f2 nil)
  (setq filename nil)
  (setq f1 (buffer-file-name (current-buffer)))
  (if (string= f1 nil)
      (setq f3  f1)
      (setq f3 (file-name-nondirectory f1))
      )
  (if (string= f3 nil)
      (setq filename (read-file-name "EPSname:"))
      (if (or
	    (string= (substring f3 -3 nil) "top")
	    (string= (substring f3 -3 nil) "tdr"))
          (setq f2 (concat (substring f3 0 -4) ".eps"))    
          (setq f2 (concat f3 ".eps")))
      (let ((insert-default-directory t))
       (setq filename (read-file-name "EPSname:" nil f2 nil f2))
       )
  )
  (setq filename (expand-file-name filename))
  (message "Writting to %s" filename)
  (setq filename (concat "\"" filename "\""))
  (setq start (point-min) end (point-max))
  (setq tdoption-ps (concat topdrawer-option-eps filename))
  (td-exec-ps start end tdoption-ps)
)

;;Main function for Topdrawer exec
(defun td-exec (start end tdoption)
  (let ((oldbuf (current-buffer))
	(msbuf (get-buffer-create "*Td Message*")))
    (set-buffer msbuf)
    (erase-buffer)
    (set-buffer oldbuf)
    (let ((process-connection-type nil))
      (if (string= topdrawer-ugsoption nil)
	  (start-process "topdrawer" "*Td Error*" topdrawer-command
		     tdoption topdrawer-option)
	  (start-process "topdrawer" "*Td Error*" topdrawer-command
	       tdoption topdrawer-extra-option topdrawer-option))
      (set-process-filter (get-process "topdrawer") 'td-message)
      (process-send-region "topdrawer" start end)
      (accept-process-output (get-process "topdrawer") 1 0)
     )
    (process-send-eof "topdrawer")
    )
)

(defun td-exec-ps (start end tdoption-ps)
  (if (string= topdrawer-ugsoption nil)
   (call-process-region start end
	 topdrawer-command nil nil nil topdrawer-option tdoption-ps)
   (call-process-region start end topdrawer-command nil nil nil
	 topdrawer-option topdrawer-extra-option tdoption-ps))
  (message "%s" "Done.")
)

;;Filter function
(defun td-message (proc string)
  (let ((oldbuf (current-buffer))
	(msbuf (get-buffer "*Td Message*")))
    (set-buffer msbuf)
    (insert string)
    (setq msflug 0)
    (if (> (count-lines (point-min) (point-max)) 1)
	(setq msflug 1)
        (message "%s" "PAUSE:")
      )
    (set-buffer oldbuf)
    (td-popup msflug)
  )
)

(defun td-popup (msflug)
  (let ((oldbuf (current-buffer))
	(msbuf (get-buffer "*Td Message*")))
    (if (save-excursion
	  (set-buffer msbuf)
	  (= msflug 1)
	  )
	(let
	 (set-buffer msbuf)
	 (setq w (prin1-to-string (one-window-p)))
	 (if (string= w "t")
	     (let (set-buffer oldbuf)
	         (split-window-vertically 10)
	         (switch-to-buffer "*Td Message*")
		 (goto-char (point-max))
                 (select-window (next-window))
                 (set-buffer oldbuf)))
	)	 
      (delete-other-windows)
     )
    )
)

;;Signal functions
(defun continue-topdrawer-process ()
  (interactive)
  (signal-process (process-id (get-process "topdrawer")) 10)
  (accept-process-output (get-process "topdrawer") 1 0)
)
(defun exit-topdrawer-process ()
  (interactive)
  (signal-process (process-id (get-process "topdrawer")) 12)
)

(defun td-debug ()
  (interactive)
  (setq start (point-min) end (point-max))
  (let ((oldbuf (current-buffer))
	(msbuf (get-buffer-create "*Td Message*")))
    (set-buffer msbuf)
    (erase-buffer)
    (set-buffer oldbuf)
    (let ((process-connection-type nil))
      (call-process-region start end "td" nil msbuf nil)
     )
    )
)

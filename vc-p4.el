;;; vc-p4.el --- Integrate Perforce support into VC mode in Emacs 21

;; Copyright (C) 2002 Curl Corporation.

;; Author: Jonathan Kamens <jik@kamens.brookline.ma.us>
;; Maintainer: Jonathan Kamens <jik@kamens.brookline.ma.us>

;; $Id$
;; The path above is on the Perforce server public.perforce.com:1666.
;; You can get this file using a P4 client talking to that depot, or
;; from the URL
;; http://public.perforce.com/guest/jonathan_kamens/vc-p4/vc-p4.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file adds support for Perforce to VC mode in Emacs 21 and any
;; other Emacs variant which uses the VC mode included in Emacs 21.
;;
;; To use this file, you also need p4-lowlevel.el somewhere in your
;; load path (or load it explicitly with "load" if it's not in your
;; load path).
;;
;; The easiest way to use this functionality is to put this file
;; somewhere in your load-path and put this in one of your init files:
;;
;;   (require 'vc-p4)
;;
;; Alternatively, as long as this file is in your load path, you can
;; customize the variable vc-handled-backends and add "P4" to it; this
;; will cause VC to load this file automatically when it is needed.
;;
;; You can't use this support and the full functionality of Rajesh
;; Vaidheeswarran's "p4.el" at the same time.  You can, however, use
;; most of p4.el's functionality by setting the customization variable
;; "p4-do-find-file" to nil.  This will prevent p4.el from attempting
;; to "take ownership" of a Perforce file when you load it, but it
;; will allow you to use all of the p4.el commands that don't apply to
;; the current buffer.

;;; Code:

(eval-when-compile
  (require 'vc)
  (require 'p4-lowlevel))

(if (not (memq 'P4 vc-handled-backends))
    (setq vc-handled-backends (cons 'P4 vc-handled-backends)))
; This is useful during development to ensure that we can simply
; reeval this buffer to get any new functions that have been added.
(put 'P4 'vc-functions nil)

; We need to fix some functions that are broken in vc.el.

(defun vc-print-log ()
  "List the change log of the current buffer in a window."
  (interactive)
  (vc-ensure-vc-buffer)
  (let* ((file buffer-file-name)
	 (use-log-view (memq (vc-backend file) '(CVS RCS SCCS))))
    (vc-call print-log file)
    (set-buffer "*vc*")
    (pop-to-buffer (current-buffer))
    (if (and use-log-view (fboundp 'log-view-mode)) (log-view-mode))
    (vc-exec-after
     `(progn
	(goto-char (point-max)) (forward-line -1)
	(while (looking-at "=*\n")
	  (delete-char (- (match-end 0) (match-beginning 0)))
	  (forward-line -1))
	(goto-char (point-min))
	(if (looking-at "[\b\t\n\v\f\r ]+")
	    (delete-char (- (match-end 0) (match-beginning 0))))
	(shrink-window-if-larger-than-buffer)
	;; move point to the log entry for the current version
	(if (and use-log-view (fboundp 'log-view-goto-rev))
	    (log-view-goto-rev ',(vc-workfile-version file))
	  (if (vc-find-backend-function ',(vc-backend file) 'show-log-entry)
	      (vc-call-backend ',(vc-backend file)
			       'show-log-entry
			       ',(vc-workfile-version file))))))))

(if (not (fboundp 'vc-default-previous-version))
    (defun vc-previous-version (rev)
      "Guess the version number immediately preceding REV."
      (if (string-match "^[0-9]+$" rev)
	  (number-to-string (- (string-to-number rev) 1))
	(let ((branch (vc-branch-part rev))
	      (minor-num (string-to-number (vc-minor-part rev))))
	  (if (> minor-num 1)
	      ;; version does probably not start a branch or release
	      (concat branch "." (number-to-string (1- minor-num)))
	    (if (vc-trunk-p rev)
		;; we are at the beginning of the trunk --
		;; don't know anything to return here
		""
	      ;; we are at the beginning of a branch --
	      ;; return version of starting point
	      (vc-branch-part branch)))))))

(if (not (fboundp 'vc-default-resolve-select-yours))
    (defun vc-maybe-resolve-conflicts (file status &optional name-A name-B)
      (vc-resynch-buffer file t (not (buffer-modified-p)))
      (if (zerop status) (message "Merge successful")
	(if (fboundp 'smerge-mode) (smerge-mode 1))
	(if (y-or-n-p "Conflicts detected.  Resolve them now? ")
	    (if (and (fboundp 'smerge-ediff)
		     (not (vc-find-backend-function (vc-backend file)
						    'resolve-select-yours)))
		(smerge-ediff)
	      (vc-resolve-conflicts name-A name-B))
	  (message "File contains conflict markers"))))

  (defun vc-default-resolve-select-yours (backend)
    (goto-char (point-min))
    (let ((found nil))
      (while (re-search-forward (concat "^<<<<<<< "
					(regexp-quote file-name) "\n") nil t)
	(setq found t)
	(replace-match "")
	(if (not (re-search-forward "^=======\n" nil t))
	    (error "Malformed conflict marker"))
	(replace-match "")
	(let ((start (point)))
	  (if (not (re-search-forward "^>>>>>>> [0-9.]+\n" nil t))
	      (error "Malformed conflict marker"))
	  (delete-region start (point))))
      found))

  (defun vc-default-resolve-select-theirs (backend)
    (goto-char (point-min))
    (while (re-search-forward (concat "^<<<<<<< "
				      (regexp-quote file-name) "\n") nil t)
      (let ((start (match-beginning 0)))
	(if (not (re-search-forward "^=======\n" nil t))
	    (error "Malformed conflict marker"))
	(delete-region start (point))
	(if (not (re-search-forward "^>>>>>>> [0-9.]+\n" nil t))
	    (error "Malformed conflict marker"))
	(replace-match "")))
    t)

  (defun vc-default-resolve-select-original (backend)
    nil)

  (defun vc-resolve-conflicts (&optional name-A name-B)
    "Invoke ediff to resolve conflicts in the current buffer.
The conflicts must be marked with rcsmerge conflict markers."
    (interactive)
    (vc-ensure-vc-buffer)
    (let* ((found nil)
	   (file-name (file-name-nondirectory buffer-file-name))
	   (backend (vc-backend buffer-file-name))
	   (your-buffer   (generate-new-buffer
			   (concat "*" file-name
				   " " (or name-A "WORKFILE") "*")))
	   (other-buffer  (generate-new-buffer
			   (concat "*" file-name
				   " " (or name-B "CHECKED-IN") "*")))
	   (ancestor-buffer (generate-new-buffer
			     (concat "*" file-name
				     " " (or name-B "ORIGINAL") "*")))
	   (result-buffer (current-buffer)))
      (save-excursion
	(set-buffer your-buffer)
	(erase-buffer)
	(insert-buffer result-buffer)
	(if (not (vc-call-backend backend 'resolve-select-yours))
	    (progn
	      (kill-buffer your-buffer)
	      (kill-buffer other-buffer)
	      (kill-buffer ancestor-buffer)
	      (error "No conflict markers found")))
      
	(set-buffer other-buffer)
	(erase-buffer)
	(insert-buffer result-buffer)
	(vc-call-backend backend 'resolve-select-theirs)

	(set-buffer ancestor-buffer)
	(erase-buffer)
	(insert-buffer result-buffer)
	(goto-char (point-min))
	(if (not (vc-call-backend backend 'resolve-select-original))
	    (progn
	      (kill-buffer ancestor-buffer)
	      (setq ancestor-buffer nil)))

	(let ((config (current-window-configuration)))

	  ;; Fire up ediff.

	  (set-buffer
	   (if ancestor-buffer
	       (ediff-merge-buffers-with-ancestor your-buffer other-buffer
						  ancestor-buffer)
	     (let ((ediff-default-variant 'default-B))
	       (ediff-merge-buffers your-buffer other-buffer))))

	  ;; Ediff is now set up, and we are in the control buffer.
        ;; Do a few further adjustments and take precautions for exit.

	  (make-local-variable 'vc-ediff-windows)
	  (setq vc-ediff-windows config)
	  (make-local-variable 'vc-ediff-result)
	  (setq vc-ediff-result result-buffer)
	  (make-local-variable 'ediff-quit-hook)
	  (setq ediff-quit-hook
		(lambda ()
		  (let ((buffer-A ediff-buffer-A)
			(buffer-B ediff-buffer-B)
			(buffer-C ediff-buffer-C)
			(result vc-ediff-result)
			(windows vc-ediff-windows))
		    (ediff-cleanup-mess)
		    (set-buffer result)
		    (erase-buffer)
		    (insert-buffer buffer-C)
		    (kill-buffer buffer-A)
		    (kill-buffer buffer-B)
		    (kill-buffer buffer-C)
		    (set-window-configuration windows)
		    (message "Conflict resolution finished; you may save the buffer"))))
	  (message "Please resolve conflicts now; exit ediff when done")
	  nil)))))

(defvar vc-p4-change-times nil
  "Alist of change numbers (represented as strings) and their age with
respect to the current time.  Set and used when annotating a Perforce
file in VC.")

(defcustom vc-p4-require-p4config nil
  "*If non-nil and the `P4CONFIG' environment variable is set, then
only perform p4 operations on a file when a P4CONFIG file can be found
in one of its parent directories.  This is useful if P4 operations are
expensive to start, e.g., if you are connect to the network over a
slow dialup connection and/or using a SSH tunnel for P4 access.  To
avoid delays when opening non-P4 files, simply set P4CONFIG as
described in the Perforce documentation, create an empty P4CONFIG file
at the root of your client workspace, and set `vc-p4-require-p4config'
to t."
  :type 'boolean
  :group 'vc)

(defcustom vc-p4-annotate-command "p4pr"
  "*Specifies the name of a command to call to annotate Perforce
files.  I recommend //guest/jonathan_kamens/p4pr.perl in the Perforce
repository public.perforce.com:1666."
  :type 'string
  :group 'vc)

(defun vc-p4-registered (file)
  "Return non-nil is FILE is handled by Perforce."
  (if (and vc-p4-require-p4config
	   (getenv "P4CONFIG")
	   (not (vc-p4-find-p4config (file-name-directory file))))
      nil
    (let ((fstat (p4-lowlevel-fstat file nil t)))
      (if (not fstat)
	  nil
	; This sets a bunch of VC properties
	(vc-p4-state file fstat)
	t))))

(defun vc-p4-state (file &optional fstat-list force)
  "Returns the current version control state of FILE in Perforce.  If
optional FSTAT-LIST is non-nil, use that list of attributes from
p4-lowlevel-fstat instead of calling it.  If optional FORCE is
non-nil, refetch all properties even if properties were previously
fetched."
  (if (and (not force) (vc-file-getprop file 'vc-p4-did-fstat))
      (vc-file-getprop file 'vc-state)
    (let* (
	   (alist (or fstat-list (p4-lowlevel-fstat file nil)))
	   (headRev (cdr (assoc "headRev" alist)))
	   (haveRev (cdr (assoc "haveRev" alist)))
	   (depotFile (cdr (assoc "depotFile" alist)))
	   (action  (cdr (assoc "action" alist)))
	   (state (if action
		      (let ((opened (p4-lowlevel-opened file)))
			(if (string-match " by \\([^@]+\\)@" opened)
			    (match-string 1 opened)
			  (if (equal headRev haveRev)
			      'edited
			    'needs-merge)))
		    (if (p4-lowlevel-diff-s file "e")
			'unlocked-changes
		      (if (equal headRev haveRev)
		      'up-to-date
		      'needs-patch))))
	   )
      (vc-file-setprop file 'vc-p4-did-fstat t)
      (vc-file-setprop file 'vc-p4-depot-file depotFile)
      (vc-file-setprop file 'vc-p4-action action)
      (vc-file-setprop file 'vc-backend 'P4)
      (vc-file-setprop file 'vc-checkout-model 'announce)
      (vc-file-setprop file 'vc-latest-version headRev)
      (vc-file-setprop file 'vc-name file)
      (vc-file-setprop file 'vc-state state)
      (vc-file-setprop file 'vc-workfile-version haveRev)
      state)))

; Here's something that would work faster, but I'm not going to
; actually try to use this unless I find that it's really too slow to
; just do all the work all the time.
;(defun vc-p4-state-heuristic (file)
;  "Estimates the current version control state of FILE in Perforce."
;  (if (and (file-exists-p file)
;	   (file-writable-p file))
;      'edited
;    'up-to-date))

(defun vc-p4-state-heuristic (file)
  (vc-p4-state file))

(defun vc-p4-dir-state (dir)
  "Determines the current version control state of the files in DIR in
Perforce and sets the appropriate VC properties."
  (let ((lists (p4-lowlevel-fstat (format "%s/*" dir) nil t))
	this-list this-file this-action)
    (if (stringp (caar lists))
	(setq lists (list lists)))
    (while lists
      (setq this-list (car lists)
	    lists (cdr lists)
	    this-file (cdr (assoc "clientFile" this-list))
	    this-action (cdr (or (assoc "action" this-list)
				 (assoc "headAction" this-list))))
      (if (and this-file
	       (not (string= this-action "delete")))
	  (vc-p4-state this-file this-list)))))

(defun vc-p4-workfile-version (file)
  "Returns the Perforce version of FILE."
  (vc-p4-state file)
  (vc-file-getprop file 'vc-workfile-version))

(defun vc-p4-latest-on-branch-p (file)
  "Returns non-nil if the Perforce version of FILE is the head
revision."
  (vc-p4-state file)
  (string= (vc-file-getprop file 'vc-latest-version)
	   (vc-file-getprop file 'vc-workfile-version)))

(defun vc-p4-checkout-model (file)
  "Returns the checkout model for Perforce (`announce')."
  'announce)

(defun vc-p4-workfile-unchanged-p (file)
  "Returns non-nil if FILE is unchanged from the version in Perforce."
  (let ((state (vc-p4-state file)))
    (and (not (equal (vc-file-getprop file 'vc-p4-action) "add"))
	 (or (equal state 'up-to-date)
	     (equal state 'needs-patch)
	     (p4-lowlevel-diff-s file "r")))))

(defun vc-p4-mode-line-string (file)
  "Return string for placement into the modeline for FILE.
Compared to the default implementation, this function handles the
special case of a Perforce file that is added but not yet committed."
  (let ((state   (vc-state file))
	(rev     (vc-workfile-version file)))
    (if (or (not rev) (string= rev "0"))
	(setq rev "@@"))
    (cond ((or (eq state 'up-to-date)
	       (eq state 'needs-patch))
	   (concat "P4-" rev))
          ((stringp state)
	   (concat "P4:" state ":" rev))
          (t
           ;; Not just for the 'edited state, but also a fallback
           ;; for all other states.  Think about different symbols
           ;; for 'needs-patch and 'needs-merge.
           (concat "P4:" rev)))))

(defun vc-p4-register (file &optional rev comment)
  (if rev
      (error "Can't specify revision when registering Perforce file."))
  (if comment
      (error "Can't specify comment when registering Perforce file."))
  (p4-lowlevel-add file))

(defun vc-p4-responsible-p (file)
  "Returns true if FILE refers to a file or directory that is
administered by Perforce."
  (if (and vc-p4-require-p4config
	   (getenv "P4CONFIG")
	   (not (vc-p4-find-p4config file)))
      nil
    (or (p4-lowlevel-fstat file nil t)
	(vc-p4-is-in-client (if (file-directory-p file)
				      (file-name-as-directory file)
				    file)))))

(defun vc-p4-checkin (file rev comment)
  "Check FILE into Perforce.  Error if REV is non-nil.  Check in with
comment COMMENT."
  (if rev
      (error "Can't specify revision for Perforce checkin."))
  (let* ((default-directory (file-name-directory file))
	 (change-buffer (p4-lowlevel-change))
	 (indent-tabs-mode 1)
	 insertion-start change-number)
    (if (vc-p4-has-unresolved-conflicts-p file)
	(error "File %s has unresolved conflicts" file))
    (save-excursion
      (set-buffer change-buffer)
      (goto-char (point-min))
      (re-search-forward "^Description:\\s-*\n")
      (kill-line 1)
      (setq insertion-start (point))
      (insert comment "\n")
      (indent-rigidly insertion-start (point) 8)
      (re-search-forward "^Files:\\s-*\n")
      (delete-region (point) (point-max))
      (insert "\t" (vc-file-getprop file 'vc-p4-depot-file) "\n")
      (setq change-number (p4-lowlevel-change (current-buffer) t))
      (p4-lowlevel-change (current-buffer) change-number)
      (p4-lowlevel-submit (current-buffer))
      ; Update its properties
      (vc-p4-state file nil t)
      (vc-mode-line file))))

(defun vc-p4-checkout (file &optional editable rev destfile)
  (if (and editable destfile (not (string= file destfile)))
      (error "Can't lock a Perforce file in an alternate location."))
  (if (string= file destfile)
      (setq destfile nil))
  (let ((default-directory (file-name-directory file))
	buffer)
    ; Make sure we've got all the current state of the file
    (vc-p4-state file)
    (cond
     ((not rev)
      (setq rev (vc-file-getprop file 'vc-workfile-version)))
     ((string= rev "")
      (setq rev (vc-file-getprop file 'vc-latest-version))))
    (if destfile
	(progn (setq buffer (p4-lowlevel-print file rev 'buffer t))
	       (set-buffer buffer)
	       (write-file destfile))
      (if (not (string= rev (vc-file-getprop file 'vc-workfile-version)))
	  (p4-lowlevel-sync file rev))
      (if editable
	  (p4-lowlevel-edit file))))
  (vc-p4-state file nil t))

(defun vc-p4-revert (file contents-done)
  "Revert FILE in Perforce.  Ignores CONTENTS-DONE."
  (p4-lowlevel-revert file)
  (vc-p4-state file nil t))

(defun vc-p4-merge (file rev1 rev2)
  "Merge changes into Perforce FILE from REV1 to REV2."
  (p4-lowlevel-integrate file file rev1 rev2 t)
  (p4-lowlevel-resolve file)
  (vc-resynch-buffer file t t)
  (vc-p4-state file nil t)
  (if (vc-p4-has-unresolved-conflicts-p file)
      1
    0))

(defun vc-p4-merge-news (file)
  "Merge new changes from Perforce into FILE."
  (p4-lowlevel-sync file)
  (p4-lowlevel-resolve file)
  (vc-resynch-buffer file t t)
  (vc-p4-state file nil t)
  (if (vc-p4-has-unresolved-conflicts-p file)
      1
    0))

(defun vc-p4-resolve-select-yours ()
  (vc-p4-select-conflict-text (current-buffer) 3))

(defun vc-p4-resolve-select-theirs ()
  (vc-p4-select-conflict-text (current-buffer) 2))

(defun vc-p4-resolve-select-original ()
  (vc-p4-select-conflict-text (current-buffer) 1))

(defun vc-p4-steal-lock (file &optional version)
  "Steal Perforce lock on FILE."
  (if (and version (not (equal version (vc-workfile-version file))))
      (error "Can't specify version when stealing Perforce lock."))
  ; Must set default-directory because this is called in a mail send
  ; hook and thus not with the current buffer set to the file being
  ; reopened.
  (let ((default-directory (file-name-directory file)))
    (p4-lowlevel-reopen file)))

(defun vc-p4-print-log (file)
  "Print Perforce log for FILE into *vc* buffer."
  (let ((inhibit-read-only t))
    (set-buffer (get-buffer-create "*vc*"))
    (erase-buffer)
    (p4-lowlevel-filelog file (current-buffer) t t)))

(defun vc-p4-show-log-entry (version)
  "Make sure Perforce log entry for VERSION is displayed in the
current buffer."
  (goto-char (point-min))
  (let (start end lines)
    (if (not (search-forward (format "\n#%s " version) nil t)) t
      (beginning-of-line)
      (setq start (point))
      (if (not (search-forward "\n#" nil t))
	  (setq end (point-max))
	(beginning-of-line)
	(setq end (point)))
      (setq lines (count-lines start end))
      (cond
       ;; if the global information and this log entry fit
       ;; into the window, display from the beginning
       ((< (count-lines (point-min) end) (window-height))
	(goto-char (point-min))
	(recenter 0)
	(goto-char start))
       ;; if the whole entry fits into the window,
       ;; display it centered
       ((< (1+ lines) (window-height))
	(goto-char start)
	(recenter (1- (- (/ (window-height) 2) (/ lines 2)))))
       ;; otherwise (the entry is too large for the window),
       ;; display from the start
       (t
	(goto-char start)
	(recenter 0))))))

(defun vc-p4-wash-log (file)
  "Remove all non-comment information from the Perforce log in the
current buffer."
  (goto-char (point-min))
  (delete-non-matching-lines "^\t"))

(defun vc-p4-update-changelog (&optional files)
  "Create ChangeLog entriers for FILES if it's non-nil, or for all
files under the default directory otherwise."
  (let ((odefault default-directory)
	(changelog (find-change-log))
	default-directory start-rev end-rev)
    (find-file-other-window changelog)
    (setq default-directory odefault)
    (goto-char (point-min))
    (if (looking-at
	 "^\\([0-9]\\{4\\}\\)[-/]\\([0-9]\\{2\\}\\)[-/]\\([0-9]\\{2\\}\\) ")
	(setq start-rev (format "@%s/%s/%s"
				(match-string 1)
				(match-string 2)
				(match-string 3))
	      end-rev "@now"))
    (if (not files)
	(setq files "..."))
    (message "Computing change log entries...")
    (insert (p4-lowlevel-info-lines
	     (p4-lowlevel-changes files nil start-rev end-rev
				  nil t nil "submitted")))
    (if (= (point) (point-min)) t
      (if (not (= (point) (point-max)))
	  (insert "\n"))
      (while (re-search-backward
	      (concat "^Change [0-9]+ on \\([0-9]+\\)/"
		      "\\([0-9]+\\)/\\([0-9]+\\) by \\(.+\\)")
	      nil t nil)
	(replace-match "\n\\1-\\2-\\3  \\4" t))
      (goto-char (point-min))
      (if (looking-at "\n")
	  (kill-line)))
    (message "Computing change log entries... done")))

(defun vc-p4-diff (file &optional rev1 rev2)
  "Do a Perforce diff into the *vc-diff* buffer."
  (let ((buffer (get-buffer-create "*vc-diff*"))
	(workfile-version (vc-file-getprop file 'vc-workfile-version))
	(inhibit-read-only t))
    (if (not rev1)
	(if (not rev2)
	    (p4-lowlevel-diff file buffer)
	  (p4-lowlevel-diff2 file file workfile-version rev2 buffer))
      (if rev2
	  (p4-lowlevel-diff2 file file rev1 rev2 buffer)
	(p4-lowlevel-diff file rev1 buffer)))))

(defun vc-p4-annotate-command (file buffer &optional version)
  "Annotate FILE into BUFFER file using `vc-p4-annotate-command'.
Annotate version VERSION if it's specified."
  (let ((full-file (if version
		       (concat file 
			       (p4-lowlevel-canonicalize-revision version))
		     file))
	(now (car (current-time)))
	log-buffer times)
    (call-process vc-p4-annotate-command
		  nil
		  buffer
		  nil
		  full-file)
    ; Calculate the date of each revision, for later
    (setq log-buffer (p4-lowlevel-filelog file nil nil t))
    (set-buffer log-buffer)
    (goto-char (point-min))
    (while (re-search-forward (concat "^#[0-9]+ change \\([0-9]+\\) .* on \\("
				      "[0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)")
			      nil t)
      (let* ((change-no (match-string 1))
	     (year (string-to-number (match-string 2)))
	     (month (string-to-number (match-string 3)))
	     (day (string-to-number (match-string 4)))
	     (then (car (encode-time 0 0 0 day month year)))
	     (difference (- now then)))
	(setq times (cons (cons change-no difference)
			  times))))
    (set-buffer buffer)
    (setq vc-p4-change-times times)))

(defun vc-p4-annotate-difference (point)
  "Returns the difference between the age of the Perforce annotation
line at point and the current time."
  (let ((regex (concat "^[[:space:]]*[[:digit:]]+[[:space:]]+"
		       "[^[:space:]]+[[:space:]]+\\([[:digit:]]+\\)"))
	match)
    (if (and (or (looking-at regex)
		 (and (re-search-forward regex nil t)
		      (forward-line 0)))
	     (setq match (assoc (match-string 1) vc-p4-change-times)))
	(cdr match)
      nil)))

(defun vc-p4-previous-version (file rev)
  "Return the Perforce revision of FILE prior to REV."
  (number-to-string (- (string-to-number rev) 1)))

(defun vc-p4-find-p4config (&optional dirname)
  "See if there is a $P4CONFIG file in DIRNAME or any of its parents.
If DIRNAME is not specified, uses `default-directory'."
  (let ((this-directory (or dirname default-directory))
	(p4config (getenv "P4CONFIG"))
	child)
    (if (not p4config)
	nil
      (catch 'found
	(while (not (equal this-directory child))
	  (if (file-exists-p (concat this-directory p4config))
	      (throw 'found (concat this-directory p4config)))
	  (setq child this-directory)
	  (setq this-directory (file-name-directory
				(directory-file-name this-directory))))))))

(defun vc-p4-is-in-client (file)
  "Return true if FILE is inside the p4 client hierarchy."
  (let* ((default-directory (file-name-directory file))
         (info (p4-lowlevel-info))
         (root-pair (assoc "Client root" info))
         (root (and root-pair (cdr root-pair)))
         (quoted-root (and root (concat "^" (regexp-quote root))))
         (cwd-pair (assoc "Current directory" info))
         (cwd (and cwd-pair (cdr cwd-pair))))
    (if (or (not quoted-root) (not (string-match quoted-root cwd)))
        nil
      (setq cwd (replace-match "" nil nil cwd))
      (if (or (string= cwd "") (string-match "^/" cwd))
          t
        nil))))

(defun vc-p4-has-unresolved-conflicts-p (file)
  "Search through FILE's buffer for unresolved P4 conflicts.
If FILE is a string, then the buffer visiting that file is searched;
no search occurs if no buffer is visiting the file.  If FILE is a
buffer, then that buffer is searched.

  Returns nil if there are no conflicts.  If there are conflicts,
returns a list of buffer positions containing the start and end of the
first conflict block in the file and then the start and end of each
subblock within it."
  (let ((buffer (if (bufferp file) file (get-file-buffer file)))
        block-start block-end block1-start block1-end block2-start block2-end
        block3-start block3-end)
    (if (not buffer)
        nil
      (save-excursion
        (save-restriction
          (set-buffer buffer)
          (widen)
          (goto-char (point-min))
          (if (not (re-search-forward "^>>>>\\( .*\\|\\)\n" nil t))
              nil
            (setq block-start (match-beginning 0)
                  block1-start (match-end 0))
            (if (not (re-search-forward "^<<<<\\( .*\\|\\)\n" nil t))
                nil
              ; Could actually be block 3, but but we'll figure that out later.
              (setq block2-end (match-beginning 0)
                    block-end (match-end 0))
              (goto-char block1-start)
              (if (not (re-search-forward "^====\\( .*\\|\\)\n" block-end t))
                  nil
                (setq block1-end (match-beginning 0)
                      block2-start (match-end 0))
                (if (not (re-search-forward "^====\\( .*\\|\\)\n" block-end t))
                    (list block-start block-end 
                          block1-start block1-end 
                          block2-start block2-end)
                  (setq block3-end block2-end
                        block2-end (match-beginning 0)
                        block3-start (match-end 0))
                  (list block-start block-end
                        block1-start block1-end
                        block2-start block2-end
                        block3-start block3-end))))))))))

(defun vc-p4-select-conflict-text (buffer which)
  "Search for P4 conflict markers in BUFFER and select the WHICH text of each.
WHICH should be either 1, 2, or 3 to indicate the first, second or
third subblock in each conflict block."
  (let (block-list block-start block-end sub-start sub-end sublist subcount
        replacement)
    (save-excursion
      (set-buffer buffer)
      (while (setq block-list (vc-p4-has-unresolved-conflicts-p buffer))
        (setq block-start (car block-list)
              block-end (cadr block-list)
              subcount which)
        (while (and block-list (> subcount 0))
          (setq block-list (cddr block-list)
                subcount (1- subcount)))
        (setq replacement (if block-list
                              (buffer-substring (car block-list) 
						(cadr block-list))
                            ""))
        (delete-region block-start block-end)
        (goto-char block-start)
        (insert replacement)))
    (if block-start t nil)))

(provide 'vc-p4)

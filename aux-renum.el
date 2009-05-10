;;; aux-renum.el --- renumber labels in LaTeX documents

;; Copyright (C) 1995, 1996 Ulrik Vieth.

;; Author: Ulrik Vieth <vieth@thphy.uni-duesseldorf.de>
;; Keywords: tex, maint
;; Version: 0.5

;;; This file is *not* part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; Description:
;;
;; This package provides two functions `aux-renumber-single' and
;; `aux-renumber-multi' to renumber equation labels and references
;; in LaTeX documents so that they match their representation in
;; the ".aux" file(s) and the formatted document.
;;
;; Why would you want to do that?  Well, consider a manuscript of
;; a book containing several hundreds of equations spread across
;; a dozen or more files.  Since it is impossible to give reasonable
;; names to so many equations, they may have been given some ad-hoc
;; labels when the manuscript was typed.  After completing some
;; intermediate or final version that will be printed and possibly
;; distributed, it may be convenient to renumber the labels so that
;; you can directly refer to the equation numbers in the printed
;; version without having to figure out what cryptic label to type
;; to produce the desired equation number in subsequent edits.
;;
;; Another appilcation might be a manuscript of a paper that will
;; be submitted electronically to a journal.  To avoid confusion
;; in subsequent correspondence with the journal editor, it might
;; be advantageous if the labels match their representation in the
;; paper copy submitted to the publisher.
;;

;; Usage:
;;
;; 1. Load or autoload this file:
;;
;;    (load-library "aux-renum")
;;
;; or
;;    (autoload 'aux-renumber-single "aux-renum"
;;      "Renumber labels in a single-file LaTeX document." t)
;;    (autoload 'aux-renumber-multi "aux-renum"
;;      "Renumber labels in a multi-file LaTeX document." t)
;;
;; 2. Run it interactively:
;;
;;   M-x aux-renumber-single
;;   M-x aux-renumber-multi
;;


;; Known Problems:
;;
;; - Operation is restricted to files in the current directory.
;;   This isn't easy to fix because the list of include files is
;;   constructed from the information in the master ".aux" file
;;   which doesn't include any directory information.
;;
;; - There are no checks to ensure that the replacement strings
;;   are unique.  This shouldn't be a problem if the ".aux" files
;;   are correct and up-to-date, but it could mess up your files
;;   if several (presumably incorrectly placed) labels have the
;;   same representation, e.g. if they refer to a section number
;;   instead of an equation number.  Check your ".aux" files or
;;   take your chances!
;;
;; - It is silently assumed that the labels in your ".aux" file
;;   follow certain conventions.  This means that every label is
;;   considered an equation label unless it starts with a known
;;   prefix.  The list of prefixes of labels that should be left
;;   unchaged is specified by the regexp `aux-exclude-regexp'.
;;
;; - The replacement function in `aux-process-tex-file' uses the
;;   high-level functions `find-file-noselect' and `save-buffer'
;;   to load and save ".tex" files, which perform all sorts of
;;   checks.  This might lead to some unexpected user interaction
;;   if some safety checks need to be answered.  This might be
;;   somewhat irritating, but it's only to be on the safe side.
;;

;; History:
;;
;; v 0.0 -- 1995/12/31  created
;; v 0.1 -- 1995/01/01  first preliminary version tested
;; v 0.2 -- 1996/01/02  first complete version tested
;; v 0.3 -- 1996/01/03  documentation added and clean up
;; v 0.4 -- 1996/01/27  minor doucmentation touch-up
;; v 0.5 -- 1196/01/28  improved search regexp for labels


;;; Code:

;; User-visible variables:

(defvar aux-translation-alist nil
  "Translation table used to renumber labels and references
in LaTeX documents.  Initialized in \\[aux-prepare-table].")

(defvar aux-exclude-regexp "\\(chap\\|sec\\|fig\\|pic\\|note\\|page\\)"
  "Regexp containing prefixes of labels to be left unchanged
when preparing `aux-translation-alist' in \\[aux-prepare-table].")


;; these variables could (or perhaps should) be local, but it's easier
;; to check their values in case something goes wrong if they're global
;;
(defvar aux-match-string nil
  "String containing the last match for a label or reference replaced
in \\[aux-prepare-tex-file].")
(defvar aux-replace-string nil
  "String containing the last replacement text for a label or reference
in \\[aux-prepare-tex-file].")


;; User-visible functions:

(defun aux-renumber-single (base-name)
  "Renumber labels and references in a single-file LaTeX document
so that they match their representation in the printed document.

Expects a string argument BASE-NAME, which is used to construct
the file names of the \".tex\" and \".aux\" file to operate on."

  (interactive "sFile name (without extension): ")
  (let* ((tex-file-name (concat base-name ".tex"))
	 (aux-file-name (concat base-name ".aux"))
	 (aux-file-list (list aux-file-name)))

    ;; prepare translation table and process the ".tex" file
    ;;
    (aux-prepare-table aux-file-list)

    (auto-save-mode -1)		; turn off auto-saving
    (message "Processing file \"%s\"" tex-file-name)
    (aux-process-tex-file tex-file-name)
    (message "Done")
    (auto-save-mode 1) 		; turn on auto-saving
    )
  )


(defun aux-renumber-multi (base-name)
  "Renumber labels and references in a multi-file LaTeX document
so that they match their representation in the printed document.

Expects a string argument BASE-NAME, which is used to construct
the file names of the main \".tex\" and \".aux\" file to operate on.
The list of file names of the included \".tex\" and \".aux\" files
is determined automatically."

  (interactive "sMaster file name (without extension): ")
  (let* (lab-buffer		; buffer to operate on
	 (tex-file-list '())
	 (aux-file-list '())
	 (main-aux-file-name (concat base-name ".aux")))

    ;; get scratch buffer to operate on
    ;;
    (setq lab-buffer (get-buffer-create "*aux-scratch*"))
    (set-buffer lab-buffer)
    (erase-buffer)		; erase left-over from previous run

    ;; insert contents of main ".aux" file
    ;;
    (if (file-readable-p main-aux-file-name)
	(insert-file-contents-literally main-aux-file-name)
      (error "I can't find file \"%s\"" main-aux-file-name))

    ;; extract file names of included ".aux" and ".tex" files
    ;;
    (delete-non-matching-lines "\\\\@input")
    (while (re-search-forward "\\\\@input{\\(.*\\)\\.aux}" nil t)
      (let ((file-name (match-string 1)))
	(setq aux-file-list
	      (cons (concat file-name ".aux") aux-file-list))
	(setq tex-file-list
	      (cons (concat file-name ".tex") tex-file-list))))
    (setq aux-file-list (reverse aux-file-list))
    (setq tex-file-list (reverse tex-file-list))

    ;; prepare translation table and process all ".tex" files
    ;;
    (aux-prepare-table aux-file-list)

    (auto-save-mode -1)		; turn off auto-saving
    (while tex-file-list
      (let ((tex-file-name (car tex-file-list)))
	(message "Processing file \"%s\"" tex-file-name)
	(aux-process-tex-file tex-file-name))
      (setq tex-file-list (cdr tex-file-list)))
    (message "Done")
    (auto-save-mode 1)		; turn on auto-saving
    )
  )


(defun aux-prepare-table (aux-file-list)
  "Prepare a translation table for renumbering LaTeX documents.

Expects an argument AUX-FILE-LIST specifying the list of files
to operate on.  Operates by extracting the information from
`\\newlabel' commands in the specified LaTeX \".aux\" file(s).

Stores the extracted information in suitable form in a scratch
buffer `*aux-scratch*' and evaluates the final buffer contents
to initialize the translation table `aux-translation-alist'."

  (let (lab-buffer)		; buffer to operate on

    ;; get scratch buffer to operate on
    ;;
    (setq lab-buffer (get-buffer-create "*aux-scratch*"))
    (set-buffer lab-buffer)
    (erase-buffer)		; erase left-over from previous run

    ;; insert contents of ".aux" file(s)
    ;;
    (goto-char (point-min))
    (while aux-file-list
      (let ((file-name (car aux-file-list)))
	(if (file-readable-p file-name)
	    (insert-file-contents-literally file-name)
	  (error "I can't find file \"%s\"" file-name)))
      (goto-char (point-max))
      (setq aux-file-list (cdr aux-file-list)))

    ;; extract all `\newlabel' commands from the ".aux" files
    ;; and discard irrelevant lines for those kinds of labels
    ;; that shouldn't be modified (e.g. sections, figures, notes)
    ;;
    (goto-char (point-min))
    ;; these functions don't move point
    (delete-non-matching-lines "\\\\newlabel")
    (delete-matching-lines aux-exclude-regexp)

    ;; convert `\newlabel' commands into suitable table form
    ;; that can be used to construct aux-translation-alist
    ;;
    (goto-char (point-min))
    (while (re-search-forward
	    "\\\\newlabel{\\(.*\\)}{{\\(.*\\)}{\\(.*\\)}}" nil t)
      (replace-match "(\"\\1\" . \"\\2\")" nil nil))

    ;; add framework for a simple lisp function
    (goto-char (point-min))
    (insert "(setq aux-translation-alist '(\n")
    (goto-char (point-max))
    (insert "))\n")

    ;; and evaluate the buffer contents in lisp
    (eval-buffer)
    )
  )


(defun aux-process-tex-file (tex-file-name)
  "Substitute arguments of `\\label' and `\\ref' commands in
a LaTeX file TEX-FILE-NAME according to the translation table
`aux-translation-alist' initialized in \\[aux-prepare-table]."

  (let (tex-buffer)		; buffer to operate on

    (setq tex-buffer (find-file-noselect tex-file-name))
    (set-buffer tex-buffer)

    ;; replace all ocurrences of `\label' and `\ref' according
    ;; to the translation table where applicable, or else leave
    ;; them unchanged by re-inserting the original match-string
    ;;
    (goto-char (point-min))
    (while (re-search-forward 
	    "\\\\\\(label\\|ref\\)[ ]*{\\([^}]+\\)}" nil t)
      (setq aux-match-string (match-string 2))
      (setq aux-replace-string
	    (or (cdr (assoc aux-match-string aux-translation-alist))
		aux-match-string))
      (replace-match "\\\\\\1" nil nil)
      (insert "{" aux-replace-string "}"))

    (save-buffer)
    )
  )

;;; aux-renum.el ends here

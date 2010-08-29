;;; swbuff-y.el --- Buffer switching code by Martin Pohlack, based
;;;                 upon swbuff-x.el and bs.el

;; Copyright (C) 2007 by Martin Pohlack

;; Author: Martin Pohlack martinp (at) gmx.de
;; Version: 0.2.1
;; Keywords: files, convenience
;; Time-stamp: <2007-07-08 19:55:25 martinp>

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is a combination of some features of swbuff-x (Kahlil (Kal)
;; HODGSON <dorge@tpg.com.au>), which is a modification to swbuff
;; (David Ponce) and bs (Olaf Sylvester <olaf@geekware.de>)
;;
;; I really liked the nicely layouted and composed buffer-selection
;; buffer of bs.  However, also fast switching buffers with one
;; key-stroke and showing a temporary buffer with a buffer list (with
;; hide timeout) a la swbuff is nice.  So I combined both's
;; advantages.
;;
;; Some current problems are documented in the code.

;;; Usage:

;; I define keyboard shortcuts and load swbuff-y as follows (init.el):
;;
;; (require 'swbuff-y)
;; (global-set-key [(meta right)] 'swbuff-switch-to-next-buffer)
;; (global-set-key [(meta left)]  'swbuff-switch-to-previous-buffer)

;; This are some usefull local settings (custom.el):
;; (custom-set-variables
;;  '(bs-attributes-list
;;    (quote (("M" 1 1 left bs--get-modified-string)
;;            ("R" 2 2 left bs--get-readonly-string)
;;            ("Buffer" bs--get-name-length 10 left bs--get-name)
;;            ("" 1 1 left " ")
;;            ("Mode" 12 12 right bs--get-mode-name)
;;            ("" 2 2 left "  ")
;;            ("File" 12 12 left bs--get-file-name)
;;            ("" 2 2 left "  "))))
;;
;;  '(bs-default-configuration "all-intern-last")
;;  '(bs-default-sort-name "by filename")
;;  '(bs-max-window-height 70))

;;


;;; Code:

(require 'swbuff-x)
(require 'bs)

(setq swbuff-display-intermediate-buffers t)

(defcustom swbuff-y-file-name-replacements
  '(("/home/user/" "~/"))
  "*List of pairs, where the first of each pair is a regexp string and
the second a replacement string.  Each pair is applied with
replace-in-string on buffer file names in the given order, so you can
get usefull shortcuts in file name lists."
  :group 'swbuff-y
  :type '(repeat sexp))

(defcustom swbuff-y-mode-name-replacements
  '(("Fundamental" "Fund.")
    ("Lisp Interaction" "Lisp I."))
  "*List of pairs, where the first of each pair is a regexp string and
the second a replacement string.  Each pair is applied with
replace-in-string on buffer mode names in the given order, so you can
shorten mode names."
  :group 'swbuff-y
  :type '(repeat sexp))

(defconst swbuff-status-buffer-name "*buffer-selection*"
  "Name of the working buffer used to display the buffer list.")

(defface swbuff-y-current-face
  '((t (:background "light green" :bold t)))
  "Face for highlighting the current buffer in swbuff-y display." )

(defface swbuff-y-header-face
  '((t (:foreground "dark blue" :bold t)))
  "Face for highlighting the header line in swbuff-y display." )

(defun swbuff-layout-status-line (window bcurr)
  "Layout a status line in WINDOW current buffer.
BCURR is the buffer name to highlight."
  (let* ((blist (bs-buffer-list))
         current
         start
         end)

    (save-selected-window
      (select-window window)
      (bs-mode)
      (setq bs-current-list blist)
      (let* ((inhibit-read-only t)
             (map-fun (lambda (entry)
                        (length (buffer-name entry))))
             (max-length-of-names (apply 'max
                                         (cons 0 (mapcar map-fun blist))))
             (name-entry-length (min bs-maximal-buffer-name-column
                                     (max bs-minimal-buffer-name-column
                                          max-length-of-names))))
        (erase-buffer)
        (setq bs--name-entry-length name-entry-length)
        (setq start (point))
        (bs--show-header)
        (setq end (point))
        (set-text-properties start end '(face swbuff-y-header-face))
        (while blist
          (setq start (point))
          (bs--insert-one-entry (car blist))
          (setq end (point))
          (when (string-equal (buffer-name (car blist)) bcurr)
            (set-text-properties
             start end '(face swbuff-y-current-face))
            (setq current start)
            )
          (insert "\n")
          (setq blist (cdr blist)))
        (delete-backward-char 1)
        (bs--set-window-height)
        (bs--goto-current-buffer)
        (bs-apply-sort-faces)
        (goto-char current)))))

;; I had to override those, because the original function would not
;; cycle through the buffer list in display-sorted order
(defun swbuff-next-buffer ()
  "Display and activate the next buffer in the buffer list."
  (let ((bufs (swbuff-y-cached-buffer-list)))  ; get curr. buffer list, sorted
				  ; and filtered as it appears in
				  ; *buffer selection*
    (setq buf bufs)
    (when buf
      (setq curh (memq (current-buffer) bufs))      ; find current buffer pos.
      (if (cadr curh)                               ; is there a next buffer ?
          (setq swbuff-current-buffer (cadr curh))  ; switch to it
        (setq swbuff-current-buffer (car bufs)))    ; or first one
      (switch-to-buffer swbuff-current-buffer t)))) ; make it so ...

(defun swbuff-previous-buffer ()
  "Display and activate the buffer at the end of the buffer list."
  (let ((bufs (swbuff-y-cached-buffer-list)))  ; get curr. buffer list, sorted
				  ; and filtered as it appears in
				  ; *buffer selection*
    (setq buf bufs)
    (when buf
      (setq last (car (last bufs)))
      (setq swbuff-current-buffer last)
      (while buf                  ; iterate over all buffers
        (when (eq (car buf) (current-buffer))  ; if we hit current buffer ...
          (setq swbuff-current-buffer last)    ; ... store the previous one
          ; break, anyone?
          )
        (setq last (car buf))
        (setq buf (cdr buf)))
      (switch-to-buffer swbuff-current-buffer t))))  ; switch to found buffer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bs stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I overrode this function to apply file-name filters
(defun bs--get-file-name (start-buffer all-buffers)
  "Return string for column 'File' in Buffer Selection Menu.
This is the variable `buffer-file-name' of current buffer.
If current mode is `dired-mode' or shell-mode it returns the
default directory.
START-BUFFER is the buffer where we started buffer selection.
ALL-BUFFERS is the list of buffer appearing in Buffer Selection Menu."
  (let ((string (copy-sequence (if (member major-mode
					   '(shell-mode dired-mode))
				   default-directory
				 (or buffer-file-name "")))))

    ;; filter file names according to map
    (setq repl-map swbuff-y-file-name-replacements)
    (while (setq repl (car repl-map))
      (setq string (replace-in-string string (car repl) (cadr repl)))
      (setq repl-map (cdr repl-map)))

    (put-text-property 0 (length string) 'mouse-face 'highlight string)
    string))

;; I overrode this function to apply mode-name filters
(defun bs--get-mode-name (start-buffer all-buffers)
  "Return the name of mode of current buffer for Buffer Selection Menu.
START-BUFFER is the buffer where we started buffer selection.
ALL-BUFFERS is the list of buffer appearing in Buffer Selection Menu."
  (let ((string mode-name))

    ;; filter mode names according to map
    (setq repl-map swbuff-y-mode-name-replacements)
    (while (setq repl (car repl-map))
      (setq string (replace-in-string string (car repl) (cadr repl)))
      (setq repl-map (cdr repl-map)))

    string))

;; I had to override this function for stable sorting (when several
;; file-less buffers where displayed with the original
;; bs--get-file-name, the display order was not stable).  For
;; stability I use the buffer name as a secondary ordering condition.
(defun bs--sort-by-filename (b1 b2)
  "Compare buffers B1 and B2 by file name and as a secondary condition
   by buffer name."
;  (if (string-equal (buffer-file-name b1) (buffer-file-name b2))
;      (string< (buffer-name b1)
;               (buffer-name b2))
;    (string< (or (buffer-file-name b1) "")
;             (or (buffer-file-name b2) ""))))

  (if (string< (buffer-file-name b1) (buffer-file-name b2))
      t
    (if (string< (buffer-file-name b2) (buffer-file-name b1))
        nil
      (if (string< (or (buffer-name b1) "")
                   (or (buffer-name b2) ""))
          t
        nil))))

(defvar __swbuff-y-cached-buffer-list nil)

(defun swbuff-y-cached-buffer-list ()
  "Return a list of buffers to be shown.  Returned a cached copy if possible."
  (if (and
       (or (eq last-command 'swbuff-switch-to-next-buffer)
           (eq last-command 'swbuff-switch-to-previous-buffer))
       __swbuff-y-cached-buffer-list)
      __swbuff-y-cached-buffer-list
    (setq __swbuff-y-cached-buffer-list (bs-buffer-list))))

(provide 'swbuff-y)
;;; swbuff-y.el ends here
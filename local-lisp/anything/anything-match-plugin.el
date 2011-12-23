;;; anything-match-plugin.el --- Humane match plug-in for anything

;; Author: rubikitch <rubikitch@ruby-lang.org>

;; Copyright (C) 2008~2011  rubikitch, all rights reserved.
;; Copyright (C) 2011, Thierry Volpiatto, all rights reserved.

;; Keywords: anything, matching
;; X-URL: <http://repo.or.cz/w/anything-config.git>

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
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Change anything.el matching algorithm humanely.
;; It gives anything.el search refinement functionality.
;; exact match -> prefix match -> multiple regexp match

;;; Commands:
;;
;; Below are complete command list:
;;
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `anything-grep-candidates-fast-directory-regexp'
;;    *Directory regexp where a RAM disk (or tmpfs) is mounted.
;;    default = nil

;; A query of multiple regexp match is space-delimited string.
;; Anything displays candidates which matches all the regexps.
;; A regexp with "!" prefix means not matching the regexp.
;; To include spaces to a regexp, prefix "\" before space,
;; it is controlled by `anything-mp-space-regexp' variable.

;; If multiple regexps are specified, first one also tries to match the source name.
;; If you want to disable this feature, evaluate
;;   (setq anything-mp-match-source-name nil) .
;; NOTE: This is obsolete and disabled in anything versions >= 1.3.7

;; This file highlights patterns like `occur'. Note that patterns
;; longer than `anything-mp-highlight-threshold' are highlighted. And
;; region out of screen is highlighted after
;; `anything-mp-highlight-delay' seconds.
;;
;; Highlight in Emacs is time-consuming process for slow computers. To
;; disable it is to set nil to `anything-mp-highlight-delay'.

;; anything-match-plugin is enable by default in anything.
;; To disable/enable it use M-x anything-c-toggle-match-plugin.

;;; Code:

(require 'anything)
(require 'cl)


;;;; Match-plugin

;;; multiple patterns
;;
;;
(defvar anything-mp-space-regexp "[\\ ] "
  "Regexp to represent space itself in multiple regexp match.")

(defun anything-mp-make-regexps (pattern)
  (if (string= pattern "")
      '("")
      (loop for s in (split-string
                      (replace-regexp-in-string anything-mp-space-regexp
                                                "\000\000" pattern)
                      " " t)
            collect (replace-regexp-in-string "\000\000" " " s))))

(defun anything-mp-1-make-regexp (pattern)
  (mapconcat 'identity (anything-mp-make-regexps pattern) ".*"))

;;; Exact match.
;;
;;
(defvar anything-mp-exact-pattern-str nil)
(defvar anything-mp-exact-pattern-real nil)

(defun anything-mp-exact-get-pattern (pattern)
  (unless (equal pattern anything-mp-exact-pattern-str)
    (setq anything-mp-exact-pattern-str pattern
          anything-mp-exact-pattern-real (concat "\n" pattern "\n")))
  anything-mp-exact-pattern-real)


(defun anything-mp-exact-match (str &optional pattern)
  (string= str (or pattern anything-pattern)))

(defun anything-mp-exact-search (pattern &rest ignore)
  (and (search-forward (anything-mp-exact-get-pattern pattern) nil t)
       (forward-line -1)))

(defun anything-mp-exact-search-backward (pattern &rest ignore)
  (and (search-backward (anything-mp-exact-get-pattern pattern) nil t)
       (forward-line 1)))


;;; Prefix match
;;
;;
(defvar anything-mp-prefix-pattern-str nil)
(defvar anything-mp-prefix-pattern-real nil)

(defun anything-mp-prefix-get-pattern (pattern)
  (unless (equal pattern anything-mp-prefix-pattern-str)
    (setq anything-mp-prefix-pattern-str pattern
          anything-mp-prefix-pattern-real (concat "\n" pattern)))
  anything-mp-prefix-pattern-real)

(defun anything-mp-prefix-match (str &optional pattern)
  (setq pattern (or pattern anything-pattern))
  (let ((len (length pattern)))
    (and (<= len (length str))
         (string= (substring str 0 len) pattern ))))

(defun anything-mp-prefix-search (pattern &rest ignore)
  (search-forward (anything-mp-prefix-get-pattern pattern) nil t))

(defun anything-mp-prefix-search-backward (pattern &rest ignore)
  (and (search-backward (anything-mp-prefix-get-pattern pattern) nil t)
       (forward-line 1)))

;;; Multiple regexp patterns 1 (order is preserved / prefix).
;;
;;
(defvar anything-mp-1-pattern-str nil)
(defvar anything-mp-1-pattern-real nil)

(defun anything-mp-1-get-pattern (pattern)
  (unless (equal pattern anything-mp-1-pattern-str)
    (setq anything-mp-1-pattern-str pattern
          anything-mp-1-pattern-real
          (concat "^" (anything-mp-1-make-regexp pattern))))
  anything-mp-1-pattern-real)

(defun* anything-mp-1-match (str &optional (pattern anything-pattern))
  (string-match (anything-mp-1-get-pattern pattern) str))

(defun anything-mp-1-search (pattern &rest ignore)
  (re-search-forward (anything-mp-1-get-pattern pattern) nil t))

(defun anything-mp-1-search-backward (pattern &rest ignore)
  (re-search-backward (anything-mp-1-get-pattern pattern) nil t))

;;; Multiple regexp patterns 2 (order is preserved / partial).
;;
;;
(defvar anything-mp-2-pattern-str nil)
(defvar anything-mp-2-pattern-real nil)

(defun anything-mp-2-get-pattern (pattern)
  (unless (equal pattern anything-mp-2-pattern-str)
    (setq anything-mp-2-pattern-str pattern
          anything-mp-2-pattern-real
          (concat "^.+" (anything-mp-1-make-regexp pattern))))
  anything-mp-2-pattern-real)

(defun* anything-mp-2-match (str &optional (pattern anything-pattern))
  (string-match (anything-mp-2-get-pattern pattern) str))

(defun anything-mp-2-search (pattern &rest ignore)
  (re-search-forward (anything-mp-2-get-pattern pattern) nil t))

(defun anything-mp-2-search-backward (pattern &rest ignore)
  (re-search-backward (anything-mp-2-get-pattern pattern) nil t))

;;; Multiple regexp patterns 3 (permutation).
;;
;;
(defvar anything-mp-3-pattern-str nil)
(defvar anything-mp-3-pattern-list nil)

(defun anything-mp-3-get-patterns (pattern)
  "Return `anything-mp-3-pattern-list', a list of predicate/regexp cons cells.
e.g ((identity . \"foo\") (identity . \"bar\")).
This is done only if `anything-mp-3-pattern-str' is same as PATTERN."
  (unless (equal pattern anything-mp-3-pattern-str)
    (setq anything-mp-3-pattern-str pattern
          anything-mp-3-pattern-list
          (anything-mp-3-get-patterns-internal pattern)))
  anything-mp-3-pattern-list)

(defun anything-mp-3-get-patterns-internal (pattern)
  "Return a list of predicate/regexp cons cells.
e.g ((identity . \"foo\") (identity . \"bar\"))."
  (unless (string= pattern "")
    (loop for pat in (anything-mp-make-regexps pattern)
          collect (if (string= "!" (substring pat 0 1))
                      (cons 'not (substring pat 1))
                      (cons 'identity pat)))))

(defun* anything-mp-3-match (str &optional (pattern anything-pattern))
  (when (stringp pattern)
    (setq pattern (anything-mp-3-get-patterns pattern)))
  (loop for (predicate . regexp) in pattern
        always (funcall predicate (string-match regexp str))))

(defun anything-mp-3-search-base (pattern searchfn1 searchfn2)
  (loop with pat = (if (stringp pattern)
                       (anything-mp-3-get-patterns pattern)
                       pattern)
        while (funcall searchfn1 (or (cdar pat) "") nil t)
        for bol = (point-at-bol)
        for eol = (point-at-eol)
        if (loop for (pred . str) in (cdr pat) always
                 (progn (goto-char bol)
                        (funcall pred (funcall searchfn2 str eol t))))
        do (goto-char eol) and return t
        else do (goto-char eol)
        finally return nil))

(defun anything-mp-3-search (pattern &rest ignore)
  (when (stringp pattern)
    (setq pattern (anything-mp-3-get-patterns pattern)))
  (anything-mp-3-search-base
   pattern 're-search-forward 're-search-forward))
  
(defun anything-mp-3-search-backward (pattern &rest ignore)
  (when (stringp pattern)
    (setq pattern (anything-mp-3-get-patterns pattern)))
  (anything-mp-3-search-base
   pattern 're-search-backward 're-search-backward))
  
;;; mp-3p- (multiple regexp pattern 3 with prefix search)
;;
;;
(defun anything-mp-3p-search (pattern &rest ignore)
  (when (stringp pattern)
    (setq pattern (anything-mp-3-get-patterns pattern)))
  (anything-mp-3-search-base
   pattern 'anything-mp-prefix-search 're-search-forward))

(defun anything-mp-3p-search-backward (pattern &rest ignore)
  (when (stringp pattern)
    (setq pattern (anything-mp-3-get-patterns pattern)))
  (anything-mp-3-search-base
   pattern 'anything-mp-prefix-search-backward 're-search-backward))

;;; source compiler
;;
;;
(defvar anything-mp-default-match-functions
  '(anything-mp-exact-match
    anything-mp-3-match))

(defvar anything-mp-default-search-functions
  '(anything-mp-exact-search
    anything-mp-3p-search
    anything-mp-3-search))

(defvar anything-mp-default-search-backward-functions
  '(anything-mp-exact-search-backward
    anything-mp-3p-search-backward
    anything-mp-3-search-backward))

(defun anything-compile-source--match-plugin (source)
  (let ((searchers (if (assoc 'search-from-end source)
                       anything-mp-default-search-backward-functions
                       anything-mp-default-search-functions)))
    `(,(if (or (assoc 'candidates-in-buffer source)
               (equal '(identity) (assoc-default 'match source)))
           '(match identity)
           `(match ,@anything-mp-default-match-functions
                   ,@(assoc-default 'match source)))
       (search ,@searchers
               ,@(assoc-default 'search source))
       ,@source)))
(add-to-list 'anything-compile-source-functions 'anything-compile-source--match-plugin t)

;;; Highlight matches.
;;
;;
(defface anything-match
    '((t (:inherit match)))
  "Face used to highlight matches."
  :group 'anything)

(defvar anything-mp-highlight-delay 0.7
  "Highlight matches with `anything-match' face after this many seconds.
 If nil, no highlight. ")

(defvar anything-mp-highlight-threshold 2
  "Minimum length of pattern to highlight.
The smaller  this value is, the slower highlight is.")

(defun anything-mp-highlight-match ()
  "Highlight matches after `anything-mp-highlight-delay' seconds."
  (when (and anything-mp-highlight-delay
             (not (string= anything-pattern "")))
    (anything-mp-highlight-match-internal (window-end (anything-window)))
    (run-with-idle-timer anything-mp-highlight-delay nil
                         'anything-mp-highlight-match-internal
                         (with-current-buffer anything-buffer (point-max)))))
(add-hook 'anything-update-hook 'anything-mp-highlight-match)

(defun anything-mp-highlight-region (start end regexp face)
  (save-excursion
    (goto-char start)
    (let (me)
      (while (and (setq me (re-search-forward regexp nil t))
                  (< (point) end)
                  (< 0 (- (match-end 0) (match-beginning 0))))
        (put-text-property (match-beginning 0) me 'face face)))))

(defun* anything-mp-highlight-match-internal (end)
  (when (anything-window)
    (set-buffer anything-buffer)
    (let ((requote (regexp-quote anything-pattern)))
      (when (>= (length requote) anything-mp-highlight-threshold)
        (anything-mp-highlight-region (point-min) end
                                      requote 'anything-match)))
    (loop for (pred . re) in (anything-mp-3-get-patterns anything-pattern)
          when (and (eq pred 'identity)
                    (>= (length re) anything-mp-highlight-threshold))
          do
          (anything-mp-highlight-region (point-min) end re 'anything-match))))

;;;; Grep-candidates plug-in

(defcustom anything-grep-candidates-fast-directory-regexp nil
  "*Directory regexp where a RAM disk (or tmpfs) is mounted.

If non-nil, grep-candidates plugin gets faster because it uses
grep as synchronous process.

ex. (setq anything-grep-candidates-fast-directory-regexp \"^/tmp/\")"
  :type 'string
  :group 'anything)

(defun agp-candidates (&optional filter)
  "Normal version of grep-candidates candidates function.
Grep is run by asynchronous process."
  (start-process-shell-command
   "anything-grep-candidates" nil
   (agp-command-line-2 filter (anything-attr-defined 'search-from-end))))

(defun agp-candidates-synchronous-grep (&optional filter)
  "Faster version of grep-candidates candidates function.
Grep is run by synchronous process.
It is faster when candidate files are in ramdisk."
  (split-string
   (shell-command-to-string
    (agp-command-line-2 filter (anything-attr-defined 'search-from-end)))
   "\n"))

(defun agp-candidates-synchronous-grep--direct-insert-match (&optional filter)
  "[EXPERIMENTAL]Fastest version of grep-candidates candidates function at the cost of absense of transformers.
Grep is run by synchronous process.
It is faster when candidate files are in ramdisk.

If (direct-insert-match) is in the source, this function is used."
  (with-current-buffer (anything-candidate-buffer 'global)
    (call-process-shell-command
     (agp-command-line-2 filter (anything-attr-defined 'search-from-end))
     nil t)))

(defun agp-command-line (query files &optional limit filter search-from-end)
  "Build command line used by grep-candidates from QUERY, FILES, LIMIT, and FILTER."
  (let ((allfiles (mapconcat (lambda (f) (shell-quote-argument (expand-file-name f)))
                             files " ")))
    (with-temp-buffer
      (when search-from-end
        (insert "tac " allfiles))
      (if (string= query "")
          (unless search-from-end
            (insert "cat " allfiles))
        (when search-from-end (insert " | "))
        (loop for (flag . re) in (anything-mp-3-get-patterns-internal query)
              for i from 0
              do
              (setq re (replace-regexp-in-string "^-" "\\-" re))
              (unless (zerop i) (insert " | ")) 
              (insert "grep -ih "
                      (if (eq flag 'identity) "" "-v ")
                      (shell-quote-argument re))
              (when (and (not search-from-end) (zerop i))
                (insert " " allfiles))))
      
      (when limit (insert (format " | head -n %d" limit)))
      (when filter (insert " | " filter))
      (buffer-string))))

(defun agp-command-line-2 (filter &optional search-from-end)
  "Build command line used by grep-candidates from FILTER and current source."
  (agp-command-line
   anything-pattern
   (anything-mklist (anything-interpret-value (anything-attr 'grep-candidates)))
   (anything-candidate-number-limit (anything-get-current-source))
   filter search-from-end))

(defun anything-compile-source--grep-candidates (source)
  (anything-aif (assoc-default 'grep-candidates source)
      (append
       source
       (let ((use-fast-directory
              (and anything-grep-candidates-fast-directory-regexp
                   (string-match
                    anything-grep-candidates-fast-directory-regexp
                    (or (car (anything-mklist (anything-interpret-value it))) "")))))
         (cond ((not (anything-interpret-value it)) nil)
               ((and use-fast-directory (assq 'direct-insert-match source))
                (anything-log "fastest version (use-fast-directory and direct-insert-match)")
                `((candidates . agp-candidates-synchronous-grep--direct-insert-match)
                  (match identity)
                  (volatile)))
               (use-fast-directory
                (anything-log "faster version (use-fast-directory)")
                `((candidates . agp-candidates-synchronous-grep)
                  (match identity)
                  (volatile)))
               (t
                (anything-log "normal version")
                '((candidates . agp-candidates)
                  (delayed))))))
    source))
(add-to-list 'anything-compile-source-functions 'anything-compile-source--grep-candidates)

(anything-document-attribute 'grep-candidates "grep-candidates plug-in"
  "grep-candidates plug-in provides anything-match-plugin.el feature with grep and head program.
It is MUCH FASTER than normal match-plugin to search from vary large (> 1MB) candidates.
Make sure to install these programs.

It expands `candidates' and `delayed' attributes.

`grep-candidates' attribute accepts a filename or list of filename.
It also accepts 0-argument function name or variable name.")

;; (anything '(((name . "grep-test")  (grep-candidates . "~/.emacs.el") (action . message))))
;; (let ((a "~/.emacs.el")) (anything '(((name . "grep-test")  (grep-candidates . a) (action . message) (delayed)))))
;; (let ((a "~/.emacs.el")) (anything '(((name . "grep-test")  (grep-candidates . (lambda () a)) (action . message) (delayed)))))
;; (anything '(((name . "grep-test")  (grep-candidates . "~/.emacs.el") (action . message) (delayed) (candidate-number-limit . 2))))
;; (let ((anything-candidate-number-limit 2)) (anything '(((name . "grep-test")  (grep-candidates . "~/.emacs.el") (action . message) (delayed)))))

;;;; unit test
;; unit test for match plugin are now in developper-tools/unit-test-match-plugin.el

(provide 'anything-match-plugin)

;;; anything-match-plugin.el ends here

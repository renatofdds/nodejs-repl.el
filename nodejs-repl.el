;;; nodejs-repl.el --- Run Node.js REPL  -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2024  Takeshi Arabiki

;; Author: Takeshi Arabiki
;; Version: 0.2.5

;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This program is derived from comint-mode and provides the following features.
;;
;;  * Token completion, same as Node.js REPL
;;  * File name completion in string
;;  * Incremental history search
;;  * Sending JavaScript code to REPL
;;
;;
;; Put this file in your Emacs Lisp path (e.g. ~/.emacs.d/site-lisp)
;; and add the following line to your .emacs:
;;
;;    (require 'nodejs-repl)
;;
;; Type M-x nodejs-repl to run Node.js REPL.
;; See also `comint-mode' to check key bindings.
;;
;; You can use `nodejs-repl-minor-mode' to send JavaScript code to REPL as
;; follows:
;;
;;     (add-hook 'js-mode-hook #'nodejs-repl-minor-mode)
;;
;; When a version manager such as nvm is used to run different versions
;; of Node.js, it is often desirable to start the REPL of the version
;; specified in the .nvmrc file per project.  In such case, customize the
;; `nodejs-repl-command` variable with a function symbol.  That function
;; should query nvm for the Node.js command to run.  For example:
;;
;;     (require 'nodejs-repl)
;;     (defun nvm-which ()
;;       (let* ((shell (concat (getenv "SHELL") " -l -c 'nvm which'"))
;;              (output (shell-command-to-string shell)))
;;         (cadr (split-string output "[\n]+" t))))
;;     (setq nodejs-repl-command #'nvm-which)
;;
;; The `nvm-which` function can be simpler, and perhaps can run faster,
;; too, if using Bash:
;;
;;     (defun nvm-which ()
;;       (let ((output (shell-command-to-string "source ~/.nvm/nvm.sh; nvm which")))
;;         (cadr (split-string output "[\n]+" t))))
;;
;;; Code:

(require 'cc-mode)
(require 'comint)
(require 'ansi-color)

(defgroup nodejs-repl nil
  "Run Node.js REPL and communicate the process."
  :group 'processes)

(defconst nodejs-repl-version "0.2.4"
  "Node.js mode Version.")

(defcustom nodejs-repl-command "node"
  "Node.js command used in `nodejs-repl-mode'.
If it is a symbol of a function, the function is called for the path of the
Node.js command. This allows to integrate with a Node.js version manager
such as nvm."
  :group 'nodejs-repl
  :type '(choice string function))

;;;###autoload
(defcustom nodejs-repl-transform-imports t
  "When non-nil, transform static imports to dynamic imports in the REPL."
  :type 'boolean
  :group 'nodejs-repl)

(defcustom nodejs-repl-arguments '()
  "Command line parameters forwarded to `nodejs-repl-command'."
  :group 'nodejs-repl
  :type '(repeat string))

(defcustom nodejs-repl-env nil
  "Alist of environment variables to be set."
  :group 'nodejs-repl
  :type '(alist :key-type string :value-type string))

(defcustom nodejs-repl-prompt "> "
  "Node.js prompt used in `nodejs-repl-mode'."
  :group 'nodejs-repl
  :type 'string)

(defcustom nodejs-repl-use-global "true"
  "`useGlobal' option of Node.js REPL method repl.start."
  :group 'nodejs-repl
  :type 'string)

(defcustom nodejs-repl-input-ignoredups t
  "If non-nil, don't add input matching the last on the input ring.

See also `comint-input-ignoredups'"
  :group 'nodejs-repl
  :type 'boolean)

(defcustom nodejs-repl-process-echoes t
  "If non-nil, Node.js does not echo any input.

See also `comint-process-echoes'"
  :group 'nodejs-repl
  :type 'boolean)

(defcustom nodejs-repl-minor-mode-lighter " Node.js-REPL"
  "Text displayed in the mode-line if `nodejs-repl-minor-mode' is active."
  :group 'nodejs-repl
  :type 'string)

(defvar nodejs-repl-nodejs-version)
(defvar nodejs-repl--nodejs-version-re
  "^v\\([0-9]+\\(?:\\.[0-9]+\\)*\\)\\(?:\\.x\\)*\\(?:-\\w+\\)?[\r\n]*$")

(defvar nodejs-repl-mode-hook nil
  "Functions runafter `nodejs-repl' is started.")

(defvar nodejs-repl-process-name "nodejs"
  "Process name of Node.js REPL.")

(defvar nodejs-repl-temp-buffer-name "*nodejs-repl-command-output*")

(defvar nodejs-repl-mode-syntax-table
  (let ((st (make-syntax-table)))
    (c-populate-syntax-table st)
    (modify-syntax-entry ?$ "_" st)
    st))

(defvar nodejs-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'completion-at-point)
    (define-key map (kbd "C-c C-c") 'nodejs-repl-quit-or-cancel)
    map))

(defvar nodejs-repl-code-format
  (concat
   "require('repl').start({prompt: '%s', useGlobal: %s, replMode: "
   "require('repl')['REPL_MODE_' + '%s'.toUpperCase()], preview: false})"))

(defvar nodejs-repl-extra-espace-sequence-re "\\(\x1b\\[[0-9]+[GJK]\\)")

(defvar nodejs-repl-ansi-color-sequence-re "\\(\x1b\\[[0-9]+m\\)")

;;; if send string like "a; Ma\t", return a; Math\x1b[1G> a; Math\x1b[0K\x1b[10G
(defvar nodejs-repl-prompt-re-format
  "\x1b\\[1G\x1b\\[0J%s.*\x1b\\[[0-9]+G.*$")
(defvar nodejs-repl-prompt-re
  (format nodejs-repl-prompt-re-format nodejs-repl-prompt nodejs-repl-prompt))
;;; not support Unicode characters
(defvar nodejs-repl-require-re
  (concat
   "\\(?:^\\|\\s-\\|[-+*/%&|><!;{}()[]\\|\\]\\)"  ; delimiter
   "require\\s-*(\\s-*"
   "\\("
   "\"[^\"\\]*\\(?:\\\\.[^\"\\]*\\)*"             ; double quote
   "\\|"
   "'[^'\\]*\\(?:\\\\.[^'\\]*\\)*"                ; single quote
   "\\)"
   "$"))

(defvar nodejs-repl-unary-operators
  '(! + - void typeof delete))

(defvar nodejs-repl-cache-token "")
(defvar nodejs-repl-cache-completions ())

(defvar nodejs-repl-get-completions-for-require-p nil)
(defvar nodejs-repl-prompt-deletion-required-p nil)

;;;--------------------------
;;; Private functions
;;;--------------------------
(defun nodejs-repl--in-string-p (&optional pos)
  "Return non-nil if POS (default point) is inside string."
  (nth 3 (syntax-ppss pos)))

(defun nodejs-repl--extract-require-argument (string)
  (if (string-match nodejs-repl-require-re string)
      (match-string 1 string)))

(defun nodejs-repl--get-last-token (string)
  "Return the last token in the STRING."
  (if (string-match "\\([._$]\\|\\w\\)+$" string)
      (match-string 0 string)))

;;; TODO:
;;; * the case that a command is sent while another command is being prossesed
;;; * the case that incomplete commands are sent like "1 +\n"
;;; * support commands which output a string without CR-LF like process.stdout.write("a")
;;;   while being processed
(defun nodejs-repl--send-string (string)
  "Send STRING to Node.js process and return the output."
  (with-temp-buffer
    (let* ((proc (get-process nodejs-repl-process-name))
           (orig-marker (marker-position (process-mark proc)))
           (orig-filter (process-filter proc))
           (orig-buf (process-buffer proc)))
      (unwind-protect
          (progn
            (set-process-buffer proc (current-buffer))
            (set-process-filter proc 'nodejs-repl--insert-and-update-status)
            (set-marker (process-mark proc) (point-min))
            (process-send-string proc string)
            (nodejs-repl--wait-for-process proc string 0.01))
        (set-process-buffer proc orig-buf)
        (set-process-filter proc orig-filter)
        (set-marker (process-mark proc) orig-marker orig-buf))
      (buffer-string))))

(defun nodejs-repl--wait-for-process (proc string interval)
  "Wait for Node.js process PROC to output all results."
  (process-put proc 'last-line "")
  (process-put proc 'running-p t)
  ;; trim trailing whitespaces
  (setq string (replace-regexp-in-string "[ \t\r\n]*\\'" "" string))
  ;; TODO: write unit test for the case that the process returns 'foo' when string is 'foo\t'
  (while (or (process-get proc 'running-p)
             (not
              (let ((last-line (process-get proc 'last-line)))
                (or (string-match-p nodejs-repl-prompt-re last-line)
                    (string-prefix-p string last-line)))))
    (process-put proc 'running-p nil)
    (accept-process-output proc interval)))

(defun nodejs-repl--insert-and-update-status (proc string)
  "Insert the output STRING and update the process PROC status (properties)
when receive the output string."
  (process-put proc 'running-p t)
  (with-current-buffer (process-buffer proc)
    (insert string)
    (goto-char (point-max))
    (process-put proc 'last-line (buffer-substring (line-beginning-position) (point)))))

(defun nodejs-repl--get-completions-from-process (token)
  "Get completions for prefix TOKEN by sending TAB to Node.js process."
  (let ((ret (progn
               ;; Send TAB twice cf. https://github.com/nodejs/node/pull/7754
               (nodejs-repl--send-string (concat token "\t"))
               (nodejs-repl--send-string "\t")))
        completions)
    (nodejs-repl-clear-line)
    (when (not (equal ret token))
      (if (string-match-p "\n" ret)
          (progn
            ;; remove extra substrings
            (setq ret (replace-regexp-in-string "\r" "" ret))
            ;; remove LF
            (setq ret (replace-regexp-in-string "\n\\{2,\\}" "\n" ret))
            ;; trim trailing whitespaces
            (setq ret (replace-regexp-in-string "[ \t\r\n]*\\'" "" ret))
            ;; don't split by whitespaces because the prompt might have whitespaces!!
            (setq completions (split-string ret "\n"))
            ;; remove the first element (input) and the last element (prompt)
            (setq completions (reverse (cdr (reverse (cdr completions)))))
            ;; split by whitespaces
            ;; '("encodeURI     encodeURIComponent") -> '("encodeURI" "encodeURIComponent")
            (setq completions (split-string
                               (replace-regexp-in-string " *$" "" (mapconcat 'identity completions " "))
                               "[ \t\r\n]+"))
            )
        (setq ret (replace-regexp-in-string nodejs-repl-extra-espace-sequence-re "" ret))
        (let ((candidate-token (nodejs-repl--get-last-token ret)))
          (setq completions (if (or (null candidate-token) (equal candidate-token token))
                                nil
                              (list candidate-token))))))
    completions))

(defun nodejs-repl--get-or-create-process ()
  (or (get-process nodejs-repl-process-name)
			(save-excursion
				(nodejs-repl)
				(get-process nodejs-repl-process-name))))

(defun nodejs-repl--filter-escape-sequnces (_string)
  "Filter extra escape sequences from output."
  (let ((beg (or comint-last-output-start
                 (point-min-marker)))
        (end (process-mark (get-buffer-process (current-buffer)))))
    (save-excursion
      (goto-char beg)
      ;; Remove ansi escape sequences used in readline.js
      (while (re-search-forward nodejs-repl-extra-espace-sequence-re end t)
        (replace-match "")))))

(defun nodejs-repl--clear-cache (_string)
  "Clear caches when outputting the result."
  (setq nodejs-repl-cache-token "")
  (setq nodejs-repl-cache-completions ()))

(defun nodejs-repl--remove-duplicated-prompt (_string)
  ;; `.load` command of Node.js repl outputs a duplicated prompt
  (let ((beg (or comint-last-output-start
                 (point-min-marker)))
        (end (process-mark (get-buffer-process (current-buffer)))))
    (save-excursion
      (goto-char beg)
      (when (re-search-forward (concat nodejs-repl-prompt nodejs-repl-prompt) end t)
        (replace-match nodejs-repl-prompt)))))

(defun nodejs-repl--delete-prompt (_string)
  ;; Redundant prompts are included in outputs from Node.js REPL
  (when nodejs-repl-prompt-deletion-required-p
    (setq nodejs-repl-prompt-deletion-required-p nil)
    (let ((beg (or comint-last-output-start (point-min-marker)))
          (end (process-mark (get-buffer-process (current-buffer)))))
      (save-excursion
        (goto-char beg)
        (forward-line 0) ; Use forward-line instead of beginning-of-line to ignore prompts
        (when (<= (point) (- end (length nodejs-repl-prompt)))
          (forward-char (length nodejs-repl-prompt))
          (while (re-search-forward nodejs-repl-prompt end t)
            (replace-match "")))))))

;; cf. https://www.ecma-international.org/ecma-262/#sec-ecmascript-language-expressions
(defun nodejs-repl--beginning-of-expression ()
  (search-backward-regexp "[[:graph:]]" nil t)
  (unless (eq (char-after) ?\;)
    (forward-char))
  (cond
   ;; Allow function
   ((and (eq (char-before) ?})
         (save-excursion
           (backward-list)
           (search-backward-regexp "[[:graph:]]" nil t)
           (and (eq (char-before) ?=) (eq (char-after) ?>))))
    (backward-list)
    (search-backward-regexp "\\(\\w\\|)\\)\\s-*=>" nil t)
    (forward-char)
    (nodejs-repl--backward-expression))
   (t
    (nodejs-repl--backward-expression)
    (while (and (not (bobp))
                (or
                 (and (eq (char-syntax (char-after)) ?\()
                      (save-excursion
                        (search-backward-regexp "[[:graph:]]" nil t)
                        (and (not (eq (char-after) ?\;))  ; e.g. otherExp; (exp)
                             (not (eq (sexp-at-point) 'return)))))  ; e.g. return (exp)
                 (save-excursion
                   (search-backward-regexp "[[:graph:]]" nil t)
                   (eq (char-after) ?.))
                 (save-excursion
                   (backward-char)
                   (eq (sexp-at-point) 'function))))
      (search-backward-regexp "[[:graph:]]" nil t)
      (when (eq (char-after) ?.)
        (search-backward-regexp "[[:graph:]]" nil t))
      (forward-char)
      (nodejs-repl--backward-expression))

    ;; e.g. !function() {}()
    (let ((exp (save-excursion
                 (search-backward-regexp "[[:graph:]]" nil t)
                 (or (sexp-at-point) (intern (char-to-string (char-after)))))))
      (when (member exp nodejs-repl-unary-operators)
				(search-backward (symbol-name exp) nil)))))
  (point))

(defun nodejs-repl--backward-expression ()
  (cond
   ((eq (char-syntax (char-before)) ?\))
    (backward-list))
   ((save-excursion
      (search-backward-regexp "[[:graph:]]" nil t)
      (eq (char-syntax (char-after)) ?w))
    (backward-sexp))
   (t
    (error "No proper expression is found backward"))))

(defun nodejs-repl--completion-at-point-function ()
  (setq nodejs-repl-prompt-deletion-required-p t)
  (when (comint-after-pmark-p)
    (let* ((input (buffer-substring (comint-line-beginning-position) (point)))
           require-arg
           token-length
           file-completion-p)
      (setq nodejs-repl-get-completions-for-require-p nil)  ;; reset
      (if (not (nodejs-repl--in-string-p))
          (setq token-length (length (nodejs-repl--get-last-token input)))
        (setq require-arg (nodejs-repl--extract-require-argument input)
              nodejs-repl-get-completions-for-require-p t)
        (if (and require-arg
                 (or (= (length require-arg) 1)  ; only quote or double quote
                     (not (string-match-p "[./]" (substring require-arg 1 2)))))  ; not file path
            (setq token-length (1- (length require-arg)))
          (let ((quote-pos (save-excursion
                             (search-backward-regexp "['\"]" (line-beginning-position) t)
                             (forward-char)
                             (point))))
            (when quote-pos
              (setq file-completion-p t
                    token-length (- (point) quote-pos))))))
      (when token-length
        (list
         (save-excursion (backward-char token-length) (point))
         (point)
         (if file-completion-p
             #'completion-file-name-table
           (completion-table-dynamic #'nodejs-repl--get-completions)))))))

(defun nodejs-repl--get-completions (token)
  (let (completions)
    (when nodejs-repl-get-completions-for-require-p
      (setq token (concat "require('" token)))
    (if (and (not (equal nodejs-repl-cache-token ""))
             (string-prefix-p nodejs-repl-cache-token token)
             (not (string-match-p (concat "^" nodejs-repl-cache-token ".*?[.(/'\"]") token)))
        (setq completions nodejs-repl-cache-completions)
      (setq completions (nodejs-repl--get-completions-from-process token)
            nodejs-repl-cache-token token
            nodejs-repl-cache-completions completions))
    completions))

(defun nodejs-repl--process-named-specifiers (self module specifiers)
  (let ((spec-list (split-string specifiers "[ \t\n\r]*,[ \t\n\r]*" t)))
    (mapcar
     (lambda (spec)
       (let* ((parts (split-string spec "[ \t\n\r]+as[ \t\n\r]+"))
							(original (string-trim (car parts)))
              (alias (and (cadr parts) (string-trim (cadr parts)))))
				 (format "  %s.%s = %s.%s ?? %s.default?.%s;" self (or alias original) module original module original)))
     (seq-filter (lambda (spec) (not (string-match-p "\\`\\(?:[ \t\n\r]*\\|\\btype[ \t\n\r]+\\S-+\\)\\'" spec))) spec-list))))

(defun nodejs-repl--import-module-statements (imports file)
	(let* ((statements (list "((g, m) => {")))
		(when imports
			(save-match-data
				"\\(?:[ \t\n\r]*\\*[ \t\n\r]+as[ \t\n\r]+\\([[:alnum:]_$]+\\)[ \t\n\r]*,?[ \t\n\r]*\\)\\(?:[ \t\n\r]*\\([[:alnum:]_$]+\\)[ \t\n\r]*,?[ \t\n\r]*\\)?\\(?:[ \t\n\r]*{[ \t\n\r]*\\([^}]+\\)[ \t\n\r]*}[ \t\n\r]*,?[ \t\n\r]*\\)"
				(when (string-match "[ \t\n\r]*{[ \t\n\r]*\\([^}]+\\)[ \t\n\r]*}" imports)
					(dolist (spec (nodejs-repl--process-named-specifiers "g" "m" (match-string 1 imports)))
						(push spec statements)))
				(when (string-match "\\(?:^[ \t\n\r]*\\([[:alnum:]_$]+\\)\\)" imports)
					(push (format "  g.%s = m.default;" (match-string 1 imports)) statements))
				(when (string-match "\\(?:[ \t\n\r]*\\*[ \t\n\r]+as[ \t\n\r]+\\([[:alnum:]_$]+\\)\\)" imports)
					(push (format "  g.%s = m;" (match-string 1 imports)) statements))
				)
			)
		(push (format "})(globalThis, await import(\"%s\"));" file) statements)

		(mapconcat #'identity (reverse statements) "\n"))
	)

(defun nodejs-repl--transform-input (code)
  (when nodejs-repl-transform-imports
		(while (string-match "^[ \t\n\r]*import[ \t\n\r]+\\([^\x00]+?\\)?[ \t\n\r]+from[ \t\n\r]+[\"']\\([^\"']+\\)[\"'][ \t\n\r]*;?$" code)
			(setq code (replace-match (nodejs-repl--import-module-statements (match-string 1 code) (match-string 2 code)) t t code)))
		(setq code
					(replace-regexp-in-string "\\(?:^[ \t\n\r]*export[ \t\n\r]+[^\x00]+?[ \t\n\r]+from[ \t\n\r]+[\"'][^\"']+[\"'][ \t\n\r]*;?$\\)\n?" "" code t t))
		(setq code
					(replace-regexp-in-string "^[ \t\n\r]*export[ \t\n\r]+" "" code t t)))
	code)

;;;--------------------------
;;; Public functions
;;;--------------------------
(defun nodejs-repl-quit-or-cancel ()
  "Send ^C to Node.js process."
  (interactive)
  (process-send-string (get-process nodejs-repl-process-name) "\x03"))

(defun nodejs-repl-clear-line ()
  "Send ^A ^K to Node.js process."
  (nodejs-repl--send-string "\x1\xb"))

;;;###autoload
(defun nodejs-repl-send-line ()
  "Send the current line to the `nodejs-repl-process'."
  (interactive)
  (save-excursion
    (let* ((proc (nodejs-repl--get-or-create-process))
           (line (buffer-substring-no-properties
									(line-beginning-position) (line-end-position))))
      (comint-send-string proc (concat (nodejs-repl--transform-input line) "\n")))))

;;;###autoload
(defun nodejs-repl-clear ()
  "Clear current repl buffer."
  (interactive)
	(with-current-buffer (process-buffer (nodejs-repl--get-or-create-process))
		(comint-clear-buffer)))

;;;###autoload
(defun nodejs-repl-send-region (start end)
  "Send the current region from START to END to the `nodejs-repl-process'."
  (interactive "r")
  (let ((proc (nodejs-repl--get-or-create-process))
        (text (buffer-substring-no-properties start end)))
    ;; Enclose the region in .editor ... EOF as this is more robust.
    ;; See: https://github.com/abicky/nodejs-repl.el/issues/17
    (comint-send-string proc ".editor\n")
    (comint-send-string proc (nodejs-repl--transform-input text))
    (comint-send-string proc "\n\x04")))

;;;###autoload
(defun nodejs-repl-send-buffer ()
  "Send the current buffer to the `nodejs-repl-process'."
  (interactive)
  (nodejs-repl-send-region (point-min) (point-max)))

;;;###autoload
(defun nodejs-repl-load-file (file)
  "Load the file to the `nodejs-repl-process'"
  (interactive (list (expand-file-name (read-file-name "Load file: " default-directory nil t buffer-file-name))))
  (let ((proc (nodejs-repl--get-or-create-process)))
    (comint-send-string proc (format ".load %s\n" file))))

;;;###autoload
(defun nodejs-repl-reload-file (bfn)
  "Load the file to the `nodejs-repl-process'"
  (interactive (list (buffer-file-name (buffer-base-buffer))))
  (when (file-exists-p bfn)
    (let ((proc (nodejs-repl--get-or-create-process)))
      (comint-send-string proc (format "delete require.cache['%s']\n" bfn))
      (comint-send-string proc (format "require('%s')\n" bfn)))))

;;;###autoload
(defun nodejs-repl-send-last-expression ()
  "Send the expression before point to the `nodejs-repl-process'."
  (interactive)
  (nodejs-repl-send-region (save-excursion (nodejs-repl--beginning-of-expression))
                           (point)))

;;;###autoload
(defun nodejs-repl-switch-to-repl ()
  "Switch to a `nodejs-repl-process', spawning a new one if necessary."
  (interactive)
  (pop-to-buffer
   (process-buffer (nodejs-repl--get-or-create-process))))

(defun nodejs-repl-execute (command &optional _buf)
  "Execute a COMMAND and output the result to the temporary buffer."
  (let ((ret (nodejs-repl--send-string (concat command "\n"))))
    (with-current-buffer (get-buffer-create nodejs-repl-temp-buffer-name)
      (erase-buffer)
      (setq ret (replace-regexp-in-string nodejs-repl-ansi-color-sequence-re "" ret))
      ;; delete inputs
      (setq ret (replace-regexp-in-string "\\(\\w\\|\\W\\)+\r\r\n" "" ret))
      (setq ret (replace-regexp-in-string "\r" "" ret))
      (insert ret)
      ;; delete last line (prompt)
      (goto-char (point-max))
      (delete-region (line-beginning-position) (point)))))

(define-derived-mode nodejs-repl-mode comint-mode "Node.js REPL"
  "Major mode for Node.js REPL."
  :syntax-table nodejs-repl-mode-syntax-table
  (set (make-local-variable 'font-lock-defaults) '(nil nil t))
  (add-hook 'comint-output-filter-functions 'nodejs-repl--delete-prompt nil t)
  (add-hook 'comint-output-filter-functions 'nodejs-repl--remove-duplicated-prompt nil t)
  (add-hook 'comint-output-filter-functions 'nodejs-repl--filter-escape-sequnces nil t)
  (add-hook 'comint-output-filter-functions 'nodejs-repl--clear-cache nil t)
  (setq comint-input-ignoredups nodejs-repl-input-ignoredups)
  (setq comint-process-echoes nodejs-repl-process-echoes)
  (add-hook 'completion-at-point-functions 'nodejs-repl--completion-at-point-function nil t)
  ;; (make-local-variable 'window-configuration-change-hook)
  ;; (add-hook 'window-configuration-change-hook 'nodejs-repl--set-prompt-deletion-required-p)
  (ansi-color-for-comint-mode-on))

;;;###autoload
(defun nodejs-repl ()
  "Run Node.js REPL."
  (interactive)
  (let ((node-command (if (and (symbolp nodejs-repl-command)
                               (functionp nodejs-repl-command))
                          (funcall nodejs-repl-command)
                        nodejs-repl-command)))
    (setq nodejs-repl-prompt-re
          (format nodejs-repl-prompt-re-format nodejs-repl-prompt nodejs-repl-prompt))
    ;; (setq nodejs-repl-nodejs-version
    ;;       ;; "v7.3.0" => "7.3.0", "v7.x-dev" => "7"
    ;;       (replace-regexp-in-string nodejs-repl--nodejs-version-re "\\1"
    ;;                                 (shell-command-to-string (concat node-command " --version"))))
    (let* ((process-environment (cl-copy-list process-environment))
           (repl-mode (or (getenv "NODE_REPL_MODE") "sloppy")))
      (cl-loop
       for (var . value) in nodejs-repl-env
       do (setenv var value))
      (pop-to-buffer
       (apply 'make-comint nodejs-repl-process-name node-command nil
              (append
               nodejs-repl-arguments
               (when nodejs-repl-code-format
                 (list "-e" (format nodejs-repl-code-format nodejs-repl-prompt nodejs-repl-use-global repl-mode))))
              ))
      (process-put (get-process nodejs-repl-process-name) 'adjust-window-size-function #'ignore)
      (nodejs-repl-mode))))

(defvar nodejs-repl-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-p" #'nodejs-repl)
    (define-key map "\C-c\C-z" #'nodejs-repl-switch-to-repl)
    (define-key map "\C-x\C-e" #'nodejs-repl-send-last-expression)
    (define-key map "\C-c\C-j" #'nodejs-repl-send-line)
    (define-key map "\C-c\C-r" #'nodejs-repl-send-region)
    (define-key map "\C-c\C-c" #'nodejs-repl-send-buffer)
    (define-key map "\C-c\C-l" #'nodejs-repl-load-file)
    map)
  "Keymap for `nodejs-repl-minor-mode'")

(easy-menu-define nodejs-repl-menu nodejs-repl-minor-mode-map "Node.js REPL menu"
  '("Node.js REPL"
    ["Start Node.js REPL" nodejs-repl
     :help "Run inferior Node.js process in a separate buffer"]
    ["Switch to REPL" nodejs-repl-switch-to-repl
     :help "Switch to running inferior Node.js process"]
    ["Eval " nodejs-repl-send-last-expression
     :help "Eval last expression in inferior Node.js session"]
    ["Eval line" nodejs-repl-send-line
     :help "Eval line in inferior Node.js session"]
    ["Eval region" nodejs-repl-send-region
     :help "Eval region in inferior Node.js session"]
    ["Eval buffer" nodejs-repl-send-buffer
     :help "Eval buffer in inferior Node.js session"]
    ["Eval file" nodejs-repl-send-file
     :help "Eval file in inferior Node.js session"]))

;;;###autoload
(define-minor-mode nodejs-repl-minor-mode
  "A minor mode for Node.js REPL"
  :group 'nodejs-repl
  :lighter nodejs-repl-minor-mode-lighter
  :keymap nodejs-repl-minor-mode-map)

(provide 'nodejs-repl)
;;; nodejs-repl.el ends here

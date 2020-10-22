;;; zenscript-common.el --- Common variables for zenscript-mode. -*- lexical-binding: t -*-

;; Copyright (c) 2020 Eutro

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; Common variables for zenscript-mode.

;;; Code:

(require 'json)
(require 'xml)

(defconst zenscript-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Symbol syntax, any ASCII character
    (modify-syntax-entry '(0 . 127) "_" table)

    ;; Word syntax
    (modify-syntax-entry '(?0 . ?9) "w" table)
    (modify-syntax-entry '(?a . ?z) "w" table)
    (modify-syntax-entry '(?A . ?Z) "w" table)
    (modify-syntax-entry ?\_ "w" table)

    ;; Whitespace
    (modify-syntax-entry ?\s " " table)
    (modify-syntax-entry ?\xa0 " " table) ; non-breaking space
    (modify-syntax-entry ?\t " " table)
    (modify-syntax-entry ?\f " " table)

    ;; Delimiters
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)

    ;; Comments
    (modify-syntax-entry ?\# "< b" table)
    (modify-syntax-entry ?\/ ". 124b" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\* ". 23" table)

    ;; Punctuation
    (modify-syntax-entry ?\= "." table)
    (modify-syntax-entry ?\+ "." table)
    (modify-syntax-entry ?\- "." table)
    (modify-syntax-entry ?\% "." table)
    (modify-syntax-entry ?\| "." table)
    (modify-syntax-entry ?\& "." table)
    (modify-syntax-entry ?\x5e "." table) ;; ^
    (modify-syntax-entry ?\? "." table)
    (modify-syntax-entry ?\: "." table)
    (modify-syntax-entry ?\~ "." table)
    (modify-syntax-entry ?\< "." table)
    (modify-syntax-entry ?\> "." table)
    (modify-syntax-entry ?\! "." table)
    (modify-syntax-entry ?\$ "." table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "\"" table)
    table))

(defconst zenscript-storage-keywords
  '("any" "bool" "byte" "short" "int" "long" "float" "double" "string" "void"))

(defconst zenscript-constants
  '("true" "false" "null"))

(defconst zenscript-math-operators
  '(".." "+=" "+" "-=" "-" "*=" "*" "/=" "/" "%=" "%" "|=" "|" "||" "&=" "&&"
    "&" "^=" "^" "?" ":" "~=" "~" "<=" "<" ">=" ">" "==" "=" "!=" "!" "$"))

(defconst zenscript-punctuation
  (append zenscript-math-operators
	  '(";" ",")))

(defconst zenscript-operator-keywords
  '("in" "has" "as" "instanceof"))

(defconst zenscript-var-keywords
  '("var" "val" "static" "global"))

(defconst zenscript-class-keywords
  '("zenClass" "frigginClass"))

(defconst zenscript-function-keywords
  '("zenConstructor" "frigginConstructor" "function"))

(defconst zenscript-preprocessors
  '("debug" "ignoreBracketErrors" "norun" "loader"
    "priority" "ikwid" "profile" "zslint" "modloaded"))

(defconst zenscript-all-keywords
  (append zenscript-var-keywords
	  zenscript-function-keywords
	  zenscript-class-keywords
	  zenscript-operator-keywords
	  '("if" "else" "version" "for" "return" "while" "break" "import")))

(defvar zenscript-dumpzs-cache ()
  "The cached data read from /ct dumpzs, or null if it should be recalculated.")

(defun zenscript--parse-dumpzs-json (loc)
  "Parse the contents of /zs dumpzs json at LOC."
  (json-read-file loc))

(defun zenscript--tag-p (tag)
  "Return a predicate which will return non-nil if EL is of type TAG."
  (lambda (el)
    (and (listp el)
	 (eq tag (car el)))))

(defun zenscript--rewrite-html (html)
  "Convert a tag HTML to a list recursively."
  (if (listp html)
      (mapcar (lambda (li)
		(let ((label (seq-find (zenscript--tag-p 'label) li)))
		  (if label
		      (cons (caddr label)
			    (zenscript--rewrite-html (seq-find (zenscript--tag-p 'ul) li)))
		    (zenscript--rewrite-html (caddr li)))))
	      (seq-filter (zenscript--tag-p 'li) html))
    html))

(defmacro ->> (x &optional form &rest more)
  "Thread the expr through the forms.

Insert X as the last item in FORM,
making a list of it if it is not a list already.

If there are MORE, insert the first form as the
last item in second form, etc."
  (cond
   ((null form) (list x))
   ((null more) (if (listp form)
                    `(,@form ,x)
                  (list form x)))
   (:else `(->> (->> ,x ,form) ,@more))))

(defun zenscript--parse-dumpzs-html (loc)
  "Parse the contents of /zs dumpzs html at LOC."
  (let ((html
	 (with-temp-buffer
	   (insert-file-contents loc)
	   (libxml-parse-html-region (point-min) (point-max) () t))))
    (->> html
	 (seq-find (zenscript--tag-p 'body))
	 (seq-find (zenscript--tag-p 'div))
	 (seq-find (zenscript--tag-p 'ul))
	 (zenscript--rewrite-html))))

(defun zenscript-calculate-dumpzs-cache (location)
  "Load the data from /zs dumpzs.

LOCATION should be a pair of the form

 (json-loc . html-loc)

json-loc:

  The location of zs_export.json, or nil.

html-loc:

  The location of tree3.html, or nil.

Hooks for 'zenscript-dumpzs-loaded are then run."
  (let ((json-loc (car location))
	(html-loc (cdr location)))
    (prog1
	(setq zenscript-dumpzs-cache
	      (cons (when json-loc (zenscript--parse-dumpzs-json json-loc))
		    (when html-loc (zenscript--parse-dumpzs-html html-loc))))
      (run-hooks 'zenscript-dumpzs-loaded))))

(defun zenscript-get-dumpzs (&optional prompt)
  "Retrieve the data dumped by /ct dumpzs.

Returns a pair in the format:

 (json . html)

json:

  The data parsed from /ct dumpzs json

html:

  The data parsed from /ct dumpzs html

If PROMPT is non-nil, the cache may be recalculated, most likely prompting the user for input."
  (or zenscript-dumpzs-cache
      (when prompt (zenscript-calculate-dumpzs-cache (zenscript-get-dumpzs-location)))))

(defun zenscript-get-dumpzs-location ()
  "Resolve zs_export.json in a parent directory, or prompt if it could not be found."
  (let ((file (buffer-file-name)))
    (when file
      (let* ((minecraft-root (locate-dominating-file file "crafttweaker.log"))
	     (json-file (when minecraft-root (concat minecraft-root "zs_export.json")))
	     (html-file (when minecraft-root (concat minecraft-root "crafttweaker_dump/tree3.html"))))
	(cons (if (and json-file (file-exists-p json-file))
		  json-file
		(condition-case _
		    (read-file-name "Location of /ct dumpzs json: " () () t)
		  (quit ())))
	      (if (and html-file (file-exists-p html-file))
		  html-file
		(condition-case _
		    (read-file-name "Location of /ct dumpzs html: " () () t)
		  (quit ()))))))))

(defun zenscript-set-dumpzs-location (location)
  "Set the location of /ct dumpzs, used for code completion.

LOCATION should be a pair of the form

 (json-loc . html-loc)

json-loc:

  The location of zs_export.json, or nil.

html-loc:

  The location of tree3.html, or nil.

When this is called, the file is loaded to compute the cache, and LOCATION isn't stored."
  (zenscript-calculate-dumpzs-cache location))

(defun zenscript--word-from (words)
  "Like `(regexp-opt WORDS)`, but wrapping with \\b."
  (concat "\\b" (regexp-opt words) "\\b"))

(defun zenscript--init-common ()
  "Initialize hooks and locals required by `zenscript-common`."
  (setq-local comment-start "//")
  (setq-local comment-start-skip "\\(\\(//+\\|#+\\)\\|/\\*+\\)\\s *")
  (setq-local comment-multi-line t)
  (set-syntax-table zenscript-mode-syntax-table)
  (set (make-local-variable 'zenscript-dumpzs-cache) (zenscript-get-dumpzs t)))

(provide 'zenscript-common)
;;; zenscript-common.el ends here

;;; zenscript-language.el --- Tools for understanding ZenScript code. -*- lexical-binding: t -*-

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

;; ZenScript language module, for parsing and understanding ZenScript.

;;; Code:

(require 'zenscript-common)

(defun zenscript--java-type-to-ztype (symbol)
  "Convert a Java type to a ZenType.

SYMBOL should be a java class name to be looked up in dumpzs."
  (car
   (seq-find (lambda (entry)
	       (equal (cadr entry) symbol))
	     (cdr (assoc "Types" (cdr (zenscript-get-dumpzs)))))))

(defun zenscript--symbol-to-type (symbol)
  "Get the ZenType from a stringified binding object SYMBOL.

If SYMBOL is the string:

 \"SymbolJavaStaticField: public static zenscript.Type ZenScriptGlobals.global\"

Then its ZenType will be resolved by looking up the zsPath of \"zenscript.Type\"."
  (when (string-match "SymbolJavaStatic\\(?:Field\\|\\(Method: JavaMethod\\)\\): public static \\(.+\\) .+$" symbol)
    (concat (if (match-string 1) "=>" "") (zenscript--java-type-to-ztype (match-string 2 symbol)))))

(defun zenscript--buffer-vals ()
  "Get a list of resolvable values in a buffer.

Returns a list of values of the form:

 (name type)

name:

  The name of the value by which it can be referenced.

type:

  The ZenType of the value, its `zsPath` from dumpzs, or nil if unknown."
  (mapcar (lambda (el)
	    (list (car el)
		  (zenscript--symbol-to-type (cadr el))))
	  (cdr (assoc "Globals" (cdr (zenscript-get-dumpzs))))))

(defun zenscript--get-importables-1 (nodes)
  "Get a list of types or static members below NODES in the tree."
  (apply 'append
	 (mapcar (lambda (node)
		   (if (stringp node)
		       (list node)
		     (let ((name (car node)))
		       ;; This operates on the assumption that type names start
		       ;; with capital letters.
		       (if (string= "Lu" (get-char-code-property (string-to-char name)
								 'general-category))
			   (cons name
				 (mapcar (lambda (member)           ; "[STATIC] "
					   (concat name "." (substring member 9)))
					 (seq-filter (lambda (member)
						       (string-match-p "\\[STATIC\\] .+" member))
						     (mapcar (lambda (node)
							       (if (stringp node)
								   node
								 (car node)))
							     (cdr node)))))
			 (mapcar (lambda (importable)
				   (concat name "." importable))
				 (zenscript--get-importables-1 (cdr node)))))))
		 nodes)))

(defun zenscript--get-members (&optional types)
  "Get the known members of the ZenTypes TYPES, or just all known members.

Returns a list of members of the following format:

 (name . extra-info)

name:

  The name of the member.

extra-info:

  A list (possibly nil) of extra information relating to the member."
  (if types
      ()
    (apply 'append
	   (mapcar (lambda (type)
		     (cdr (assoc 'members type)))
		   (cdr (assoc 'zenTypeDumps (car (zenscript-get-dumpzs))))))))

(defun zenscript--get-importables ()
  "Get a list of all things that can be imported: static members and types.

Returns a list of type names that can be imported."
  (zenscript--get-importables-1 (cdr (assoc "Root (Symbol Package)" (cdr (zenscript-get-dumpzs))))))

;;; Parsing:

;; I have written these too many times, so I'm keeping them around.

(defun zenscript--skip-ws-and-comments ()
  "Skip across any whitespace characters or comments."
  (skip-syntax-forward " >")
  (when (char-after)
    (let ((ppss (save-excursion
		  (parse-partial-sexp (point)
				      (+ (point) 2)
				      () () () t))))
      (when (nth 4 ppss)
	(parse-partial-sexp (point)
			    (point-max)
			    () () ppss 'syntax-table)
	(zenscript--skip-ws-and-comments)))))

(defun zenscript--looking-at-backwards-p (regex)
  "Return non-nil if searching REGEX backwards ends at point."
  (= (point)
     (save-excursion
       (or (and (re-search-backward regex (point-min) t)
		(match-end 0))
	   0))))

(defun zenscript--tokenize-buffer (&optional from to no-error)
  "Read the buffer into a list of tokens.

FROM is the start position, and defaults to `point-min`.

TO is the end position, and defaults to `point-max`.

If a token is unrecognised, and NO-ERROR is nil, an error is thrown.
If NO-ERROR is non-nil, then parsing stops instead, returning the partially
accumulated list of tokens, and leaving point where it is.

If parsing concludes, then point is left at TO.

Note: this uses the syntax table to handle comments."
  (goto-char (or from (point-min)))
  (let ((to (or to (point-max)))
	(continue t)
	tokens)
    (zenscript--skip-ws-and-comments)
    (while (and continue (char-after))
      (let ((start (point))
	    (next-token (zenscript--next-token)))
	(when (or (>= (point) to)
		  (not next-token))
	  (setq continue ()))
	(if next-token
	    (if (> (point) to)
		(goto-char start)
	      (setq tokens (cons next-token tokens))
	      (when (< (point) to)
		(zenscript--skip-ws-and-comments)))
	  (unless no-error
	    (error "%s" "Unrecognised token")))))
    (reverse tokens)))

(defun zenscript--next-token (&optional skip-whitespace)
  "Parse the next ZenScript token after point.

If SKIP-WHITESPACE is non-nil, whitespace and comments
are skipped according to `syntax-table`.

Return a pair of the form

 (type . val)

or nil if no token was recognised.

Where type is the type of the token, as seen here
https://docs.blamejared.com/1.12/en/Dev_Area/ZenTokens/
and val is the string value of the token.

point is put after token, if one was found."
  (let ((begin (point)))
    (when skip-whitespace (zenscript--skip-ws-and-comments))
    (if-let ((type (cond ((looking-at (regexp-quote "{")) 'T_AOPEN)
			 ((looking-at (regexp-quote "}")) 'T_ACLOSE)
			 ((looking-at (regexp-quote "[")) 'T_SQBROPEN)
			 ((looking-at (regexp-quote "]")) 'T_SQBRCLOSE)
			 ((looking-at (regexp-quote "..")) 'T_DOT2)
			 ((looking-at (regexp-quote ".")) 'T_DOT)
			 ((looking-at (regexp-quote ",")) 'T_COMMA)
			 ((looking-at (regexp-quote "+=")) 'T_PLUSASSIGN)
			 ((looking-at (regexp-quote "+")) 'T_PLUS)
			 ((looking-at (regexp-quote "-=")) 'T_MINUSASSIGN)
			 ((looking-at (regexp-quote "-")) 'T_MINUS)
			 ((looking-at (regexp-quote "*=")) 'T_MULASSIGN)
			 ((looking-at (regexp-quote "*")) 'T_MUL)
			 ((looking-at (regexp-quote "/=")) 'T_DIVASSIGN)
			 ((looking-at (regexp-quote "/")) 'T_DIV)
			 ((looking-at (regexp-quote "%=")) 'T_MODASSIGN)
			 ((looking-at (regexp-quote "%")) 'T_MOD)
			 ((looking-at (regexp-quote "|=")) 'T_ORASSIGN)
			 ((looking-at (regexp-quote "|")) 'T_OR)
			 ((looking-at (regexp-quote "||")) 'T_OR2)
			 ((looking-at (regexp-quote "&=")) 'T_ANDASSIGN)
			 ((looking-at (regexp-quote "&&")) 'T_AND2)
			 ((looking-at (regexp-quote "&")) 'T_AND)
			 ((looking-at (regexp-quote "^=")) 'T_XORASSIGN)
			 ((looking-at (regexp-quote "^")) 'T_XOR)
			 ((looking-at (regexp-quote "?")) 'T_QUEST)
			 ((looking-at (regexp-quote ":")) 'T_COLON)
			 ((looking-at (regexp-quote "(")) 'T_BROPEN)
			 ((looking-at (regexp-quote ")")) 'T_BRCLOSE)
			 ((looking-at (regexp-quote "~=")) 'T_TILDEASSIGN)
			 ((looking-at (regexp-quote "~")) 'T_TILDE)
			 ((looking-at (regexp-quote ";")) 'T_SEMICOLON)
			 ((looking-at (regexp-quote "<=")) 'T_LTEQ)
			 ((looking-at (regexp-quote "<")) 'T_LT)
			 ((looking-at (regexp-quote ">=")) 'T_GTEQ)
			 ((looking-at (regexp-quote ">")) 'T_GT)
			 ((looking-at (regexp-quote "==")) 'T_EQ)
			 ((looking-at (regexp-quote "=")) 'T_ASSIGN)
			 ((looking-at (regexp-quote "!=")) 'T_NOTEQ)
			 ((looking-at (regexp-quote "!")) 'T_NOT)
			 ((looking-at (regexp-quote "$")) 'T_DOLLAR)
			 ((looking-at (regexp-quote "any")) 'T_ANY)
			 ((looking-at (regexp-quote "bool")) 'T_BOOL)
			 ((looking-at (regexp-quote "byte")) 'T_BYTE)
			 ((looking-at (regexp-quote "short")) 'T_SHORT)
			 ((looking-at (regexp-quote "int")) 'T_INT)
			 ((looking-at (regexp-quote "long")) 'T_LONG)
			 ((looking-at (regexp-quote "float")) 'T_FLOAT)
			 ((looking-at (regexp-quote "double")) 'T_DOUBLE)
			 ((looking-at (regexp-quote "string")) 'T_STRING)
			 ((looking-at (regexp-quote "function")) 'T_FUNCTION)
			 ((looking-at (regexp-quote "in")) 'T_IN)
			 ((looking-at (regexp-quote "has")) 'T_IN)
			 ((looking-at (regexp-quote "void")) 'T_VOID)
			 ((looking-at (regexp-quote "as")) 'T_AS)
			 ((looking-at (regexp-quote "version")) 'T_VERSION)
			 ((looking-at (regexp-quote "if")) 'T_IF)
			 ((looking-at (regexp-quote "else")) 'T_ELSE)
			 ((looking-at (regexp-quote "for")) 'T_FOR)
			 ((looking-at (regexp-quote "return")) 'T_RETURN)
			 ((looking-at (regexp-quote "var")) 'T_VAR)
			 ((looking-at (regexp-quote "val")) 'T_VAL)
			 ((looking-at (regexp-quote "while")) 'T_WHILE)
			 ((looking-at (regexp-quote "break")) 'T_BREAK)
			 ((looking-at (regexp-quote "null")) 'T_NULL)
			 ((looking-at (regexp-quote "true")) 'T_TRUE)
			 ((looking-at (regexp-quote "false")) 'T_FALSE)
			 ((looking-at (regexp-quote "import")) 'T_IMPORT)
			 ((looking-at (regexp-quote "global")) 'T_GLOBAL)
			 ((looking-at (regexp-quote "static")) 'T_STATIC)
			 ((looking-at (regexp-quote "instanceof")) 'T_INSTANCEOF)
			 ((looking-at (regexp-quote "zenClass")) 'T_ZEN_CLASS)
			 ((looking-at (regexp-quote "frigginClass")) 'T_ZEN_CLASS)
			 ((looking-at (regexp-quote "zenConstructor")) 'T_ZEN_CONSTRUCTOR)
			 ((looking-at (regexp-quote "frigginConstructor")) 'T_ZEN_CONSTRUCTOR)
			 ((or (looking-at "-?\\(0\\|[1-9][0-9]*\\)")
			      (looking-at "0x[a-fA-F0-9]*"))
			  'T_INTVALUE)
			 ((looking-at "-?\\(0\\|[1-9][0-9]*\\)\\.[0-9]+\\([eE][+-]?[0-9]+\\)?[fFdD]?")
			  'T_FLOATVALUE)
			 ((or (looking-at "'\\([^'\\\\]\\|\\\\\\(['\"\\\\/bfnrt]\\|u[0-9a-fA-F]{4}\\)\\)*?'")
			      (looking-at "\"\\([^\"\\\\]\\|\\\\\\(['\"\\\\/bfnrt]\\|u[0-9a-fA-F]{4}\\)\\)*\""))
			  'T_STRINGVALUE)
			 ((looking-at "[a-zA-Z_][a-zA-Z_0-9]*")
			  'T_ID))))
	(progn (goto-char (match-end 0))
	       (cons type (match-string 0)))
      (goto-char begin)
      ())))

(provide 'zenscript-language)
;;; zenscript-language.el ends here

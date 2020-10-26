;;; zenscript-mode.el --- Major mode for ZenScript. -*- lexical-binding: t -*-

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

;; Major mode for ZenScript.

;;; Code:

(require 'json)
(require 'zenscript-common)
(require 'zenscript-highlighting)
(require 'zenscript-completion)
(require 'zenscript-indentation)
(require 'zenscript-language)

(defgroup zenscript nil
  "Major mode for editing ZenScript code."
  :prefix "zenscript-"
  :group 'languages)

(defconst zenscript-mode-version
  "0.1.0-SNAPSHOT"
  "The current version of `zenscript-mode`.")

(defun zenscript-view-docs ()
  "Open the CraftTweaker docs in your default browser."
  (interactive)
  (let ((ver (completing-read "Game version (default: 1.12): "
			      zenscript-game-versions
			      () () () () "1.12"))
	(loc (completing-read "Select your language (default: en): "
			      zenscript-docs-languages
			      () () () () "en")))
    (browse-url (concat zenscript-docs-base-url ver "/" loc))))

(defconst zenscript-docs-base-url "https://docs.blamejared.com/"
  "The base URL for the official CraftTweaker docs.")

(defconst zenscript-game-versions '("1.12" "1.14" "1.15" "1.16"))

(defconst zenscript-docs-languages '("de" "en" "es" "fr" "it" "ja" "ko" "pl" "ru" "zh"))

(defconst zenscript-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-c d") 'zenscript-view-docs)
    keymap))

(define-derived-mode zenscript-mode prog-mode "ZenScript"
  "Major mode for ZenScript."
  (use-local-map zenscript-mode-map)
  (zenscript--init-common)
  (zenscript--init-highlighting)
  (zenscript--init-indents)
  (zenscript--init-completion)
  (zenscript--init-language))

(add-to-list 'auto-mode-alist '("\\.zs\\'" . zenscript-mode))

(provide 'zenscript-mode)
;;; zenscript-mode.el ends here

;;; layer-xah-fly-keys.el --- Package description (don't include the word "Emacs")  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 First Last

;; Author: First Last <name@example.com>
;; URL: https://example.com/layer-xah-fly-keys.el
;; Version: 0.1-pre
;; Package-Requires: ((emacs "25.2") (xah-fly-keys "18.1.20220921110635"))
;; Keywords: something

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package allows flanges to be easily frobnicated.

;;;; Installation

;;;;; MELPA

;; If you installed from MELPA, you're done.

;;;;; Manual


;;;; Tips

;; + You can customize settings in the `layer-xah-fly-keys' group.

;;;; Credits

;; This package would not have been possible without the following
;; packages: foo[1], which showed me how to bifurcate, and bar[2],
;; which takes care of flanges.
;;
;;  [1] https://example.com/foo.el
;;  [2] https://example.com/bar.el

;;; Code:

;;;; Requirements

;; (require 'foo)
;; (require 'bar)

;;;; Customization

(defgroup layer-xah-fly-keys nil
  "Set up several option for xah-fly-key"
  :link '(url-link "https://example.com/layer-xah-fly-keys.el"))

;;;; Variables
;; (defvar  &optional INITVALUE "DOCSTRING")

;;;; Commands



;;;; Functions

;;;;; Public

(defun layer-xah-fly-keys-add-keys-to-keymap-name (KeymapName place command
							   &optional DirectQ)
  "Add a key binding to the keymap named KEYMAPNAME.

The key binding will bind the key represented by PLACE to the COMMAND.
If DIRECTQ is non-nil, then PLACE will be interpreted as a direct key
sequence. Otherwise, PLACE will be converted to a key sequence using
`xah-fly--key-char'.

The key binding will be created using `define-key'. The binding will
be evaluated using `eval' before it is added to KEYMAPNAME."
  (eval
   `(define-key
      ,KeymapName
      (kbd (,(if DirectQ #'identity #'xah-fly--key-char)
            ,place
            ))
      ,(list 'quote command))))

(defun layer-xah-fly-keys-create-variable-and-function (name choices)
  "Function used to create a variable and the associated function. This allows to add the function in the keymap of xah thanks to the `layer-xah-fly-keys-add-keys-to-keymap-name` function, where the function calls directly the variable.
Moreover, this variable allows to let the user choose the function to call via
  the customization menu.
Return the symbol of the function.
Choices are a list of symbols that the user can choose. The base value is the first symbol in this list"
  (let ((name-of-the-variable (concat "layer-xah-fly-key-" name "-variable"))
	(name-of-the-function (concat "layer-xah-fly-key-" name
				      "-function")))
    ;; the variable
    (eval (car (read-from-string
		(format
		 "(defcustom %s #'%s
      \"%s\"
      :type '(choice %s)
      :group 'layer-xah-fly-keys)\n"
		 name-of-the-variable
		 (car choices)
		 (format "Function to call instead of `%s' in xah-fly-keys. This is the variable associated with the function `%s'
" name name-of-the-function)
		 (mapconcat (lambda (x) (format "(function-item %s)" x)) choices " ")))))

    ;; the function
    (eval (car (read-from-string
		(format
		 "(defun %s ()
      \"%s\"
  (interactive)
  (call-interactively %s))\n"
		 name-of-the-function
		 (format "
Function to be called instead of `%s' in xah-fly-keys.
This function calls the function associated with the value of the variable `%s'" name name-of-the-variable)
		 name-of-the-variable))))))

(layer-xah-fly-keys-add-keys-to-keymap-name 
'xah-fly-command-map "m" (layer-xah-fly-keys-create-variable-and-function "isearch-forward" 
						 '(consult-line cp/consult-ripgrep-with-directory)))
;; (setq layer-xah-fly-key-isearch-forward-variable 'isearch-forward)


(layer-xah-fly-keys-add-keys-to-keymap-name
 'xah-fly-Rp2p1-key-map "h"
 (layer-xah-fly-keys-create-variable-and-function "recentf-open-files"
						  '(recentf-open-files consult-recent-file)))

(layer-xah-fly-keys-add-keys-to-keymap-name 
'xah-fly-leader-key-map "9" (layer-xah-fly-keys-create-variable-and-function "ispell-word" 
						 '(ispell-word
						   flyspell-correct-wrapper)))

(layer-xah-fly-keys-add-keys-to-keymap-name 
'xah-fly-command-map "'" (layer-xah-fly-keys-create-variable-and-function "xah-extend-selection" 
						 '(xah-extend-selection er/expand-region)))



;;;;; Private



;;;; Footer

(require)

(provide 'layer-xah-fly-keys)

;;; layer-xah-fly-keys.el ends here

;;; xah-fly-keys-layer-better-remap.el --- Key-binding for each mode in xah-fly-keys -*- lexical-binding: t -*-

;; Copyright (C) 2021, 2022  Free Software Foundation, Inc.

;; Author: Payard Clément <payard.clement63@gmail.com>
;; Maintainer: Payard Clément <payard.clement63@gmail.com>
;; Created: 2022
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (xah-fly-keys-layer "0.1"))

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; This package aim to help the user to extand xah-fly-key to map a prefix ke, 
;;; Code:

;; prefix key, voir le paragraphe dans Readme.org pour comprendre
;;changer la variable ici pour changer la touche de la major mode !


(defun xah-fly-keys-layer-create-variable-and-function (name choices)
  "Function used to create a variable and the associated function. This allows to add the function in the keymap of xah thanks to the `xah-fly-keys-layer-add-keys-to-keymap-name` function, where the function calls directly the variable.
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
      :group 'xah-fly-keys-layer)\n"
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

(xah-fly-keys-layer-add-keys-to-keymap-name 
'xah-fly-command-map "m" (xah-fly-keys-layer-create-variable-and-function "isearch-forward" 
						 '(consult-line cp/consult-ripgrep-with-directory)))
;; (setq layer-xah-fly-key-isearch-forward-variable 'isearch-forward)


(xah-fly-keys-layer-add-keys-to-keymap-name
 'xah-fly-Rp2p1-key-map "h"
 (xah-fly-keys-layer-create-variable-and-function "recentf-open-files"
						  '(recentf-open-files consult-recent-file)))

(xah-fly-keys-layer-add-keys-to-keymap-name 
'xah-fly-leader-key-map "9" (xah-fly-keys-layer-create-variable-and-function "ispell-word" 
						 '(ispell-word
						   flyspell-correct-wrapper)))

(xah-fly-keys-layer-add-keys-to-keymap-name 
'xah-fly-command-map "'" (xah-fly-keys-layer-create-variable-and-function "xah-extend-selection" 
						 '(xah-extend-selection er/expand-region)))


(provide 'xah-fly-keys-layer-better-remap)
;;; xah-fly-keys-layer-better-remap.el ends here

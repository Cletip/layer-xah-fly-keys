;;; xah-fly-keys-layer.el --- Package description (don't include the word "Emacs")  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 First Last

;; Author: First Last <name@example.com>
;; URL: https://example.com/xah-fly-keys-layer.el
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

;; + You can customize settings in the `xah-fly-keys-layer' group.

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

(defgroup xah-fly-keys-layer nil
  "Set up several option for xah-fly-key"
  :link '(url-link "https://example.com/xah-fly-keys-layer.el"))

;;;; Variables
;; (defvar  &optional INITVALUE "DOCSTRING")

;;;; Commands



;;;; Functions

;;;;; Public

(defun xah-fly-keys-layer-add-keys-to-keymap-name (KeymapName place command
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


;;;;; Private



;;;; Footer

(provide 'xah-fly-keys-layer)

;;; xah-fly-keys-layer.el ends here

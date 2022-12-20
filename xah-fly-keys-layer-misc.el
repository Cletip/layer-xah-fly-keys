;;; xah-fly-keys-layer-misc.el --- Key-binding for each mode in xah-fly-keys -*- lexical-binding: t -*-

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

;; This package adds various useful features to xah-fly-keys
;;; Code:

(require 'xah-fly-keys)

(defvar xah-fly-keys-layer-misc-enter-open-line nil "When t, open-line in
xah-fly-keys can do the job exactly like <enter> too")

(when xah-fly-keys-layer-misc-enter-open-line

  (defun xah-fly-keys-layer-misc-enter-open-line (_x)
    "DOCSTRING"
    (define-key xah-fly-command-map (xah-fly--convert-kbd-str "o")
      (if (or buffer-read-only
              (string-equal major-mode "minibuffer-mode")
              ;; (string-equal major-mode "org-agenda-mode")
              ;; (string-equal major-mode "fundamental-mode")
              )
          (kbd "RET")
	'open-line)))

  (add-to-list 'window-state-change-functions
	       'xah-fly-keys-layer-misc-enter-open-line))

(defvar xah-fly-keys-layer-misc-autosave nil "If t, when enter in the command
mode in xah-fly-keys, the buffer is saved")

(when xah-fly-keys-layer-misc-autosave

  (defvar xah-fly-keys-layer-misc-autosave-exclude '(gpg test) "List of mode to
exclude the auto-save functionnality of xah-fly-keys-layer-misc")

  (defun xah-fly-keys-layer-misc-autosave ()
    "Save current buffer if it's a file and if the current mode is not in the
    list of `xah-fly-keys-layer-misc-save-mode-exclude'"
    (interactive)
    (when (and
	   (buffer-file-name) ;;check if it's a file
	   ;; (not (string-equal (file-name-extension buffer-file-name) "gpg"))
	   (not (member major-mode xah-fly-keys-layer-misc-save-mode-exclude))) ;; check
      ;;if it's not in the list
      (save-buffer)))

  (add-hook 'xah-fly-command-mode-activate-hook 'xah-fly-keys-layer-misc-autosave))

(provide 'xah-fly-keys-layer-misc)
;;; xah-fly-keys-layer-misc.el ends here

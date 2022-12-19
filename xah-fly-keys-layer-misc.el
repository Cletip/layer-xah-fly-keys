;;; xah-fly-keys-layer-misc.el.el --- Auto activate command or insert mode -*- lexical-binding: t -*-

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
  (add-to-list 'window-state-change-functions (lambda (_x)
						(define-key xah-fly-command-map (xah-fly--convert-kbd-str "o")
						  (if (or buffer-read-only
							  (string-equal major-mode "minibuffer-mode")
							  ;; (string-equal major-mode "org-agenda-mode")
							  ;; (string-equal major-mode "fundamental-mode")
							  )
						      (kbd "RET")
						    'open-line)))))



(provide 'xah-fly-keys-layer-misc)
;;; xah-fly-keys-layer-misc.el ends here

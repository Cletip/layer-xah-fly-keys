;;; xah-fly-keys-layer-auto-mode.el --- Auto activate command or insert mode -*- lexical-binding: t -*-

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

;; This package is intended to provide the user with functions to enable or disable the insertion or control mode of xah-fly-keys automatically
;;; Code:

(require 'xah-fly-keys-layer)

(defvar xah-fly-keys-layer-auto-command-mode-functions '()
  "List of functions to automatically call xah-fly-command-mode-activate on.")

(defvar xah-fly-keys-layer-auto-insert-mode-functions '()
  "List of functions to automatically call xah-fly-insert-mode-activate on.")

(defun xah-fly-keys-layer-auto-command-mode-activate ()
  "Wires xah-fly-command-mode-activate to all functions from cp/xfk-auto-command-mode-fns."
  (dolist (element xah-fly-keys-layer-auto-command-mode-functions)
    (advice-add element :after #'xah-fly-command-mode-activate)))

(defun xah-fly-keys-layer-auto-insert-mode-activate ()
  "Wires xah-fly-insert-mode-activate to all functions from cp/xfk-auto-insert-mode-fns."
  (dolist (element xah-fly-keys-layer-auto-insert-mode-functions)
    (advice-add element :after #'xah-fly-insert-mode-activate)))

(provide 'xah-fly-keys-layer-auto-mode)
;;; xah-fly-keys-layer-auto-mode.el ends here

;;; xah-fly-keys-layer-personal-keymap.el --- Key-binding for each mode in xah-fly-keys -*- lexical-binding: t -*-

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

;; This package provides, on SPC SPC, an additional configuration layer for the user
;;; Code:

(require 'xah-fly-keys-layer)

;; Spc Spc
(xah-fly--define-keys
 (define-prefix-command 'xah-fly-keys-layer-personal-key-map)
 '(
   ;; ("RET" . nil)
   ;; ("<up>"  . nil)
   ;; ("<down>"  . nil)
   ;; ("'" . nil)
   ;; ("," . nil)
   ;; ("." . nil)
   ;; ("0" . nil)
   ;; ("1" . nil)
   ;; ("2" . nil)
   ;; ("3" . nil)
   ;; ("4" . nil)
   ;; ("5" . nil)
   ;; ("6" . nil)
   ;; ("7" . nil)
   ;; ("8" . nil)
   ;; ("9" . nil)

   ;; ("a" . nil)
   ;; ("b" . nil)
   ;; ("c" . nil)
   ;; ("d" . nil)
   ;; ("d" . nil)
   ;; ("d" . nil)
   ;; ("e" . nil)
   ;; ("f" . nil)
   ;; ("g" . nil)
   ;; ("h" . nil)
   ;; ("i" . nil)
   ;; ("j" . nil)
   ;; ("k" . nil)
   ;; ("l" . nil)
   ;; ("m" . nil)
   ;; ("n" . nil)
   ;; ("o" . nil)
   ;; ("p" . nil)
   ;; ("q" . nil)
   ;; ("r" . nil)
   ;; ("s" . nil)
   ;; ("t" . nil)
   ;; ("u" . nil)
   ;; ("v" . nil)
   ;; ("w" . nil)
   ;; ("x" . nil)
   ;; ("y" . nil)
   ;; ("z" . nil)
   ))

;; add the personnal keymap to SPC SPC
(xah-fly-keys-layer-add-keys-to-keymap-name 'xah-fly-leader-key-map "SPC" 'xah-fly-keys-layer-personal-key-map)

(provide 'xah-fly-keys-layer-personal-keymap)
;;; xah-fly-keys-layer-personal-keymap.el ends here

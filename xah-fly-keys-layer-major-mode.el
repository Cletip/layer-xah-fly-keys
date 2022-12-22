;;; xah-fly-keys-layer-major-mode.el --- Key-binding for each mode in xah-fly-keys -*- lexical-binding: t -*-

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

(require 'xah-fly-keys)

(defvar xah-fly-keys-layer-major-mode-key (xah-fly--convert-kbd-str "i") "Where to place the key to enter the shortcuts of the mode")

(defun xah-fly-keys-layer-no-major-mode ()
  (message "There is no custom keymap for the major mode  %s. You can do a pull request" major-mode)
  )

(defun xah-fly-keys-layer-major-mode-change (&rest args)
  "Call different commands depending on what's current major mode."
  (interactive)
  ;;pour que ça marche, necessite un argument utilisé ici. Mais enlevé avec les autres messages pour pas que se soit moche
  ;; (message "wrapper called %s" args)
  (cond
   ((string-equal major-mode "minibuffer-mode")
    (define-key xah-fly-command-map (kbd xah-fly-keys-layer-major-mode-key) 'xah-fly-keys-layer-minibuffer-mode-keymap))
   ((string-equal major-mode "org-mode")
    (define-key xah-fly-command-map (kbd xah-fly-keys-layer-major-mode-key) 'xah-fly-keys-layer-org-mode-keymap))
   ((string-equal major-mode "org-agenda-mode")
    (define-key xah-fly-command-map (kbd xah-fly-keys-layer-major-mode-key) 'xah-fly-keys-layer-org-agenda-mode-keymap))
   ((string-equal major-mode "java-mode")
    (define-key xah-fly-command-map (kbd xah-fly-keys-layer-major-mode-key) 'xah-fly-keys-layer-java-mode-keymap))
   ((string-equal major-mode "c-mode")
    (define-key xah-fly-command-map (kbd xah-fly-keys-layer-major-mode-key) 'xah-fly-keys-layer-c-mode-keymap))
   ((string-equal major-mode "xah-elisp-mode")
    (define-key xah-fly-command-map (kbd xah-fly-keys-layer-major-mode-key) 'xah-fly-keys-layer-elisp-mode-keymap))
   ((string-equal major-mode "emacs-lisp-mode")
    (define-key xah-fly-command-map (kbd xah-fly-keys-layer-major-mode-key) 'xah-fly-keys-layer-elisp-mode-keymap))
   (t
    (define-key xah-fly-command-map (kbd xah-fly-keys-layer-major-mode-key) 'xah-fly-keys-layer-no-major-mode))))

;; Load the right mode
(add-to-list 'window-state-change-functions 'xah-fly-keys-layer-major-mode-change)

(if (>= emacs-major-version 28)
    (add-to-list 'window-state-change-functions
		 'xah-fly-keys-layer-major-mode-change)
  (progn
    (add-to-list 'window-buffer-change-functions #'xah-fly-keys-layer-major-mode-change)
    (add-to-list 'window-selection-change-functions #'xah-fly-keys-layer-major-mode-change)
    (add-hook 'window-selection-change-functions #'xah-fly-keys-layer-major-mode-change)))

;;old : 
;; (add-to-list 'window-buffer-change-functions #'cp-major-mode)
;; (add-to-list 'window-selection-change-functions #'cp-major-mode)
;; (add-hook 'window-selection-change-functions #'cp-major-mode)

;; TODO add mode
(xah-fly--define-keys
 (define-prefix-command 'xah-fly-keys-layer-org-mode-keymap)
 '(

   ("SPC" . org-mode-babel-keymap)

   ;; ("-" . "^") NOTE: this is a dead key
   ("'" . org-table-create-or-convert-from-region)
   ("," . org-mark-element)
   ("." . org-todo)
   (";" . org-toggle-narrow-to-subtree)
   ;; ("/" . "x")

   ;; ("[" . "=")
   ;; ("]" . "%")

   ;; ("=" . "ç")

   ("a" . org-export-dispatch)
   ;; ("b" . org-goto)
   ("b" . consult-org-heading) ;; mieux
   ("c" . org-insert-link)
   ("L" . org-store-link)
   ("d" . org-mode-keymap-movement)
   ("e" . org-meta-return)
   ;; ("E" . org-insert-todo-heading)
   ("f" . org-roam-ref-add)
   ("g" . org-roam-buffer-toggle)
   ("h" . vulpea-insert)
   ;; ("i" . ",")
   ("j" . org-deadline)
   ("k" . org-schedule)
   ("l" . "cp-vulpea-buffer-tags-remove-BROUILLON")
   ;; ("m" . org-insert-todo-heading)
   ("n" . vulpea-tags-add)
   ("o" . org-refile)
   ("p" . org-set-tags-command)
   ("q" . org-sort)
   ("r" . vulpea-meta-add)
   ("s" . citar-insert-citation)
   ;; ("t" . vulpea-find-backlink)
   ;; ("u" . org-capture-keymap) ;; TODO, mis dans SPC SPC
   ;; ("u" . org-capture)  ;; TODO changer
   
   ("v" . org-insert-todo-heading)
   ;; ("v" . cp-vulpea-meta-fait-add)
   ("w" . consult-org-roam-forward-links)
   ("x" . org-time-stamp)
   ;; ("y" . "b")
   ;; ("z" . "v")
   ))


;; org-agenda
(xah-fly--define-keys
 (define-prefix-command 'xah-fly-keys-layer-org-agenda-mode-keymap)
 '(
   ;; ("a" . mark-whole-buffer)
   ;; ("b" . end-of-buffer)
   ("c" . org-agenda-set-tags)
   ("d" . org-mode-action-keymap)
   ;; ("e" . xah-fly-e-keymap)
   ;; ("f" . xah-search-current-word)
   ("g" . org-agenda-open-link)
   ("h" . org-agenda-todo)
   ;; ("i" . kill-line)
   ;; ("j" . xah-copy-all-or-region)
   ;; ("j" . winner-undo)
   ;; ("k" . xah-paste-or-paste-previous)
   ;; ("l" . recenter-top-bottom)
   ("m" . org-refile-goto-last-stored)
   ("n" . org-agenda-refile)
   ;; ("o" . exchange-point-and-mark)
   ;; ("p" . query-replace)
   ;; ("q" . xah-cut-all-or-region)
   ("r" . org-insert-link)
   ;; ("s" . save-buffer)
   ;; ("s" . winner-undo);;touche dispo
   ;; ("s" . major-mode-hydra) ;;perso
   ("t" . org-agenda-schedule)
   ;; ("u" . switch-to-buffer)
   ;; v
   ("w" . org-capture-goto-last-stored)
   ;; ("x" . xah-toggle-letter-case)
   ;; ("x" . xah-toggle-previous-letter-case)
   
   ;; ("y" . popup-kill-ring)
   ("z" . org-agenda-archive)
   )
 )

(provide 'xah-fly-keys-layer-major-mode)
;;; xah-fly-keys-layer-major-mode.el ends here

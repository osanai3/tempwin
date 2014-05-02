;;; tempwin.el --- make temporary window -*- lexical-binding: t; -*-

;; Copyright (C) 2014 by Koichi Osanai

;; Author: Koichi Osanai <osanai3@gmail.com>
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'cl-lib)

(defvar tempwin-window-parameters
  '(
    tempwin-suicide-function
    tempwin-parent-window
    tempwin-ignore-selected
    tempwin-delete-window-list
    )
  "persistent parameters")

(defun tempwin-set-suicide-function (window function)
  (set-window-parameter window 'tempwin-suicide-function function))

(defun tempwin-get-suicide-function (window)
  (window-parameter window 'tempwin-suicide-function))

(defun tempwin-set-parent-window (child parent)
  (set-window-parameter child 'tempwin-parent-window parent))

(defun tempwin-get-parent-window (window)
  (window-parameter window 'tempwin-parent-window))

(defun tempwin-set-ignore-selected (window ignore-selected)
  (set-window-parameter window 'tempwin-ignore-selected ignore-selected))

(defun tempwin-get-ignore-selected (window)
  (window-parameter window 'tempwin-ignore-selected))

(defun tempwin-push-delete-window-list (window window-to-be-deleted)
  (push window-to-be-deleted (window-parameter window 'tempwin-delete-window-list)))

(defun tempwin-delete-window (window)
  "Delete window registered by `tempwin-push-delete-window-list'.
Return deleted window or nil if no window is deleted."
  (when (window-parameter window 'tempwin-delete-window-list)
    (let ((head (pop (window-parameter window 'tempwin-delete-window-list))))
      (cond
       ((eq head window) (delete-window window) window)
       ((window-live-p head) (tempwin-push-delete-window-list window head) (tempwin-delete-window head))
       (t (tempwin-delete-window window))))))


(defun tempwin-tempp (window)
  (tempwin-get-suicide-function window))

(defun tempwin-root-window (window)
  (let ((parent (tempwin-get-parent-window window)))
    (if parent (tempwin-root-window parent) window)))

(defun tempwin-create-child-window (parent size side ignore-selected frame-pop)
  (let ((base-window (if frame-pop (frame-root-window parent) (tempwin-root-window parent))))
    (let ((child (split-window  base-window (- size) side)))
      (tempwin-set-parent-window child parent)
      (tempwin-set-suicide-function
       child
       (tempwin-create-suicide-function child)
       )
      (tempwin-set-ignore-selected child ignore-selected)
      (tempwin-push-delete-window-list parent child)
      (tempwin-push-delete-window-list child child)
      child
      )))

(defun tempwin-create-suicide-function (window)
  (lambda ()
    (unless (tempwin-alivep window)
      (delete-window window)
      (tempwin-delete-windows))))

(defun tempwin-alivep (window)
  (and
   (window-live-p (tempwin-get-parent-window window))
   (or (tempwin-get-ignore-selected window)
       (tempwin-descendantp window (selected-window))
       (eq (minibuffer-window) (selected-window))
       )
   )
  )

(defun tempwin-descendantp (ancestor descendant)
  (or (eq ancestor descendant)
      (let ((parent (tempwin-get-parent-window descendant)))
        (when parent (tempwin-descendantp ancestor parent)))))

(defun tempwin-display-buffer-alist-function (buffer alist)
  (unless (get-buffer-window-list buffer)
    (let ((size (cdr (assoc 'size alist)))
          (side (cdr (assoc 'side alist)))
          (ignore-selected (or (cdr (assoc 'ignore-selected alist)) nil))
          (frame-pop (cdr (assoc 'frame-pop alist))))
      (with-selected-window
          (tempwin-create-child-window (selected-window) size side ignore-selected frame-pop)
        (switch-to-buffer buffer nil t)
        (selected-window)
        ))))

(defun tempwin-keyboard-quit ()
  (interactive)
  (unless (tempwin-delete-window (selected-window)) (keyboard-quit)))

(defvar tempwin-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap keyboard-quit] 'tempwin-keyboard-quit)
    map))

(defun tempwin-delete-windows ()
  (mapcar 'funcall (delq nil (mapcar 'tempwin-get-suicide-function (window-list)))))

(define-minor-mode tempwin-minor-mode "delete window with C-g" :global t)

(defun tempwin-start ()
  (interactive)
  (tempwin-minor-mode 1)
  (add-hook 'buffer-list-update-hook 'tempwin-delete-windows)
  (mapcar
   (lambda (param-name)
     (push (cons param-name t) window-persistent-parameters))
   tempwin-window-parameters)
  t)

(defun tempwin-stop ()
  (interactive)
  (remove-hook 'buffer-list-update-hook 'tempwin-delete-windows)
  (tempwin-minor-mode 0)
  (mapcar
   (lambda (param-name)
     (setq window-persistent-parameters (assq-delete-all param-name window-persistent-parameters)))
   tempwin-window-parameters)
  t)

(provide 'tempwin)

;;; tempwin.el ends here
(setq display-buffer-alist
      (list
       (cons
        "^\\*magit:.*\\*$"
        (cons
         'tempwin-display-buffer-alist-function
         '((side . above) (size . 10))
        ))
       (cons
        "^\\*eshell\\*$"
        (cons
         'tempwin-display-buffer-alist-function
         '((side . below) (size . 15))
        ))
       (cons
        "^\\*IBuffer\\*$"
        (cons
         'tempwin-display-buffer-alist-function
         '((side . left) (size . 25) (frame-pop . t))
        ))
       (cons
        "^\\*Help\\*$"
        (cons
         'tempwin-display-buffer-alist-function
         '((side . below) (size . 15))
         ))
       (cons
        "^\\*Completions\\*$"
        (cons
         'tempwin-display-buffer-alist-function
         '((side . below) (size . 10) (ignore-selected . t) (frame-pop . t))
        )
       )))
;(eval-buffer)
;(tempwin-start)
;(tempwin-stop)

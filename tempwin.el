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
    tempwin-dedicated-buffer
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

(defun tempwin-set-dedicated-buffer (window buffer)
  (set-window-parameter window 'tempwin-dedicated-buffer buffer))

(defun tempwin-get-dedicated-buffer (window)
  (window-parameter window 'tempwin-dedicated-buffer))

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

(defun tempwin-create-child-window (parent buffer size side ignore-selected frame-pop dedicated)
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
      (with-selected-window child (switch-to-buffer buffer nil t))
      (when dedicated
        (tempwin-set-dedicated-buffer child buffer)
        (set-window-parameter
         child
         'delete-window
         (lambda (window)
           (set-window-parameter window 'delete-window t)
           (unless (eq (window-buffer window) buffer)
             (tempwin-copy-buffer-to-parent-window window))
           (when (window-live-p window) (delete-window window)))))
      (set-window-parameter child 'delete-other-windows 'tempwin-copy-buffer-to-parent-window)
      child
      )))

(defun tempwin-create-suicide-function (window)
  (lambda ()
    (when (and (window-live-p window) (not (tempwin-alivep window)))
      (delete-window window)
      (tempwin-delete-windows))))

(defun tempwin-alivep (window)
  (and
   (window-live-p (tempwin-get-parent-window window))
   (or (tempwin-get-ignore-selected window)
       (tempwin-descendantp window (selected-window))
       (eq (minibuffer-window) (selected-window))
       )
   (or (not (tempwin-dedicated-windowp window))
       (tempwin-valid-dedicated-windowp window)
       )
  ))

(defun tempwin-dedicated-windowp (window)
  (tempwin-get-dedicated-buffer window))

(defun tempwin-valid-dedicated-windowp (window)
  (and (tempwin-dedicated-windowp window)
       (eq (window-buffer window) (tempwin-get-dedicated-buffer window))))

(defun tempwin-copy-buffer-to-parent-window (window)
  (let ((parent (tempwin-get-parent-window window)))
    (when parent
      (with-selected-window parent
        (switch-to-buffer (window-buffer window))))))

(defun tempwin-descendantp (ancestor descendant)
  (or (eq ancestor descendant)
      (let ((parent (tempwin-get-parent-window descendant)))
        (when parent (tempwin-descendantp ancestor parent)))))

(defun tempwin-display-buffer-alist-function (buffer alist)
  (unless (get-buffer-window-list buffer)
    (let ((size (cdr (assoc 'size alist)))
          (side (cdr (assoc 'side alist)))
          (ignore-selected (or (cdr (assoc 'ignore-selected alist)) nil))
          (frame-pop (cdr (assoc 'frame-pop alist)))
          (dedicated (cdr (assoc 'dedicated alist)))
          )
      (with-selected-window
          (tempwin-create-child-window (selected-window) buffer size side ignore-selected frame-pop dedicated)
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

(defvar tempwin-timer nil)
(defcustom tempwin-timer-interval 0.1 "timer interval")

(defun tempwin-translate-to-buffer-alist (input)
  (let* (
         (fix-alist
          (lambda (list)
            (mapcar
             (lambda (item) (if (atom item) (cons item t) item))
             list)))
         (translate
          (lambda (list)
            (destructuring-bind (regexp &rest alist) list
              (cons
               regexp
               (cons
                'tempwin-display-buffer-alist-function
                (funcall fix-alist alist)))))))
    (mapcar translate input)))

(defcustom tempwin-display-buffer-alist
  (tempwin-translate-to-buffer-alist
   '(
     ("^\\*eshell\\*$" (side . below) (size . 15))
     ("^\\*IBuffer\\*$" (side . left) (size . 25) frame-pop dedicated)
     ("^\\*Buffer List\\*$" (side . left) (size . 25) frame-pop dedicated)
     ("^\\*Help\\*$" (side . below) (size . 15))
     ("^\\*Completions\\*$" (side . below) (size . 10) ignore-selected frame-pop dedicated)
     ))
   "append this list to display-buffer-alist when tempwin-start")

(defvar tempwin-original-display-buffer-alist nil)

(defun tempwin-start ()
  (interactive)
  (tempwin-minor-mode 1)
  (add-hook 'buffer-list-update-hook 'tempwin-delete-windows)
  (mapcar
   (lambda (param-name)
     (push (cons param-name t) window-persistent-parameters))
   tempwin-window-parameters)
  (unless tempwin-timer
    (setq tempwin-timer (run-with-timer tempwin-timer-interval tempwin-timer-interval 'tempwin-delete-windows)))
  (unless tempwin-original-display-buffer-alist
    (setq tempwin-original-display-buffer-alist display-buffer-alist)
    (setq display-buffer-alist (append tempwin-display-buffer-alist display-buffer-alist)))
  t)

(defun tempwin-stop ()
  (interactive)
  (remove-hook 'buffer-list-update-hook 'tempwin-delete-windows)
  (tempwin-minor-mode 0)
  (mapcar
   (lambda (param-name)
     (setq window-persistent-parameters (assq-delete-all param-name window-persistent-parameters)))
   tempwin-window-parameters)
  (cancel-timer tempwin-timer)
  (setq tempwin-timer nil)
  (setq display-buffer-alist tempwin-original-display-buffer-alist)
  (setq tempwin-original-display-buffer-alist nil)
  t)

(provide 'tempwin)

;;; tempwin.el ends here
;(eval-buffer)
;(tempwin-start)
;(tempwin-stop)

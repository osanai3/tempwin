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

(defun tempwin-set-timer-callback (window function)
  (set-window-parameter window 'tempwin-timer-callback function))

(defun tempwin-get-timer-callback (window)
  (window-parameter window 'tempwin-timer-callback))

(defun tempwin-set-parent-window (child parent)
  (set-window-parameter child 'tempwin-parent-window parent))

(defun tempwin-get-parent-window (window)
  (window-parameter window 'tempwin-parent-window))

(defun tempwin-set-lifetime (window lifetime)
  (set-window-parameter
   window
   'tempwin-live-until
   (time-add (current-time) (list 0 lifetime 0 0)))
  )

(defun tempwin-in-lifetimep (window)
  (time-less-p
   (current-time)
   (window-parameter window 'tempwin-live-until)))

(defun tempwin-push-delete-window-list (window window-to-be-deleted)
  (push window-to-be-deleted (window-parameter window 'tempwin-delete-window-list)))

(defun tempwin-delete-window (window)
  "Delete window registered by `tempwin-push-delete-window-list'.
Return deleted window or nil if no window is deleted."
  (when (window-parameter window 'tempwin-delete-window-list)
    (let ((head (pop (window-parameter window 'tempwin-delete-window-list))))
      (cond
       ((window-live-p head) (delete-window head) head)
       (t (tempwin-delete-window window))))))


(defun tempwin-tempp (window)
  (tempwin-get-timer-callback window))

(defun tempwin-timer-function ()
  (mapcar 'funcall (delq nil (mapcar 'tempwin-get-timer-callback (window-list)))))

(defun tempwin-create-child-window (parent size side lifetime)
  (let ((child (split-window parent (- size) side)))
    (tempwin-set-parent-window child parent)
    (tempwin-set-timer-callback
     child
     (tempwin-create-timer-callback child)
     )
    (tempwin-set-lifetime child lifetime)
    (tempwin-push-delete-window-list parent child)
    (tempwin-push-delete-window-list child child)
    child
    ))

(defun tempwin-create-timer-callback (window)
  (lambda ()
    (unless (tempwin-alivep window) (delete-window window))))

(defun tempwin-alivep (window)
  (and
   (window-live-p (tempwin-get-parent-window window))
   (or (tempwin-in-lifetimep window)
       (tempwin-descendantp window (selected-window))
       (eq (minibuffer-window) (selected-window))
       )
   )
  )

(defun tempwin-delete-window-if-parent-is-deleted (window)
  (unless (window-live-p (tempwin-get-parent-window window))
    (delete-window window)))

(defun tempwin-descendantp (ancestor descendant)
  (or (eq ancestor descendant)
      (let ((parent (tempwin-get-parent-window descendant)))
        (when parent (tempwin-descendantp ancestor parent)))))

(defun tempwin-delete-window-unless-descendant-is-selected (window)
  (unless (tempwin-descendantp window (selected-window))
    (delete-window window)))

(defun tempwin-display-buffer-alist-function (buffer alist)
  (unless (get-buffer-window-list buffer)
    (let ((size (cdr (assoc 'size alist)))
          (side (cdr (assoc 'side alist)))
          (lifetime (or (cdr (assoc 'lifetime alist)) 0)))
      (with-selected-window
          (tempwin-create-child-window (selected-window) size side lifetime)
        (switch-to-buffer buffer nil t)
        (selected-window)
        ))))

(defun tempwin-keyboard-quit ()
  (interactive)
  (unless (tempwin-delete-window (selected-window)) (keyboard-quit)))

(defvar tempwin-timer nil)
(defcustom tempwin-timer-interval 0.1 "timer interval")

(defvar tempwin-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap keyboard-quit] 'tempwin-keyboard-quit)
    map))

(define-minor-mode tempwin-minor-mode "delete window with C-g" :global t)

(defun tempwin-start ()
  (interactive)
  (tempwin-minor-mode 1)
  (unless tempwin-timer
    (setq tempwin-timer (run-with-timer tempwin-timer-interval tempwin-timer-interval 'tempwin-timer-function))))

(defun tempwin-stop ()
  (interactive)
  (cancel-timer tempwin-timer)
  (setq tempwin-timer nil)
  (tempwin-minor-mode 0))

(provide 'tempwin)

;;; tempwin.el ends here
(setq display-buffer-alist
      (list
       (cons
        "^\\*magit:.*\\*$"
        (cons
         'tempwin-display-buffer-alist-function
         '((side . above) (size . 10)))
        )
       (cons
        "^\\*eshell\\*$"
        (cons
         'tempwin-display-buffer-alist-function
         '((side . below) (size . 15)))
        )
       ;(cons
        ;"^\\*Help\\*$"
       ;(cons
        ; 'tempwin-display-buffer-alist-function
         ;'((side . below) (size . 15)))
       ;)
       ;(cons
        ;"^\\*Completions\\*$"
        ;(cons
         ;'tempwin-display-buffer-alist-function
       ;'((side . below) (size . 10) (lifetime . 1000)))
       ;)
       ))
;(eval-buffer)
(tempwin-start)
(tempwin-stop)

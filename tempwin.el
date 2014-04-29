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

(defun tempwin-tempp (window)
  (tempwin-get-timer-callback window))

(defun tempwin-timer-function ()
  (let ((callbacks nil))
    (walk-window-tree
     (lambda (window)
       (let ((callback (tempwin-get-timer-callback window)))
         (when callback (push callback callbacks)))))
    (mapcar 'funcall callbacks)))

(defun tempwin-create-child-window (parent size side lifetime)
  (let ((child (split-window parent (- size) side)))
    (tempwin-set-parent-window child parent)
    (tempwin-set-timer-callback
     child
     (lambda ()
       (unless (tempwin-in-lifetimep child)
         (tempwin-delete-window-unless-descendant-is-selected child)
         )
       (tempwin-delete-window-if-parent-is-deleted child)
       ))
    (tempwin-set-lifetime child lifetime)
    child
    ))

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
  (let ((size (cdr (assoc 'size alist)))
        (side (cdr (assoc 'side alist)))
        (lifetime (or (cdr (assoc 'lifetime alist)) 0)))
    (with-selected-window
        (tempwin-create-child-window (selected-window) size side lifetime)
      (switch-to-buffer buffer nil t)
      (selected-window)
      )))

(defvar tempwin-timer)
(defcustom tempwin-timer-interval 0.1 "timer interval")

(defvar tempwin-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap keyboard-quit] (lambda () (interactive) (if (tempwin-tempp (selected-window)) (delete-window) (keyboard-quit))))
    map
    ))

(define-minor-mode tempwin-minor-mode "delete window with C-g" :global t)

(defun tempwin-start ()
  (interactive)
  (tempwin-minor-mode 1)
  (unless tempwin-timer
    (setq tempwin-timer (run-with-idle-timer tempwin-timer-interval t 'tempwin-timer-function))))

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
         '((side . below) (size . 10)))
        )
       (cons
        "^\\*eshell\\*$"
        (cons
         'tempwin-display-buffer-alist-function
         '((side . below) (size . 15)))
        )
       (cons
        "^\\*Completions\\*$"
        (cons
         'tempwin-display-buffer-alist-function
         '((side . below) (size . 10) (lifetime . 3)))
        )
       ))
(tempwin-start)
(tempwin-stop)

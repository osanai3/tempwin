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
  (set-window-parameter window 'tempwin-timer-callback function)
)

(defun tempwin-get-timer-callback (window)
  (window-parameter window 'tempwin-timer-callback)
)

(defun tempwin-set-parent-window (child parent)
  (set-window-parameter child 'tempwin-parent-window parent)
)

(defun tempwin-get-parent-window (window)
  (window-parameter window 'tempwin-parent-window)
)

(defun tempwin-timer-function ()
  (let ((callbacks nil))
    (walk-window-tree
     (lambda (window)
       (let ((callback (tempwin-get-timer-callback window)))
         (when callback (push callback callbacks)))))
    (mapcar 'funcall callbacks)))

(defun tempwin-create-child-window (parent size side)
  (let ((child (split-window parent (- size) side)))
    (tempwin-set-parent-window child parent)
    (tempwin-set-timer-callback child (lambda () (tempwin-delete-window-unless-descendant-is-selected child)))
    child
    ))

(defun tempwin-descendantp (ancestor descendant)
  (or (eq ancestor descendant)
      (let ((parent (tempwin-get-parent-window descendant)))
        (when parent (tempwin-descendantp ancestor parent)))))

(defun tempwin-delete-window-unless-descendant-is-selected (window)
  (unless (tempwin-descendantp window (selected-window))
    (delete-window window)))

(defun tempwin-display-buffer-alist-function (buffer alist)
  (let ((size (cdr (assoc 'size alist)))
        (side (cdr (assoc 'side alist))))
    (with-selected-window
        (tempwin-create-child-window (selected-window) size side)
      (switch-to-buffer buffer nil t)
      (selected-window)
      )))

(provide 'tempwin)

;;; tempwin.el ends here

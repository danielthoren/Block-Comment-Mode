;;; block-comment-util.el --- General, uncategorized, utilities   -*- lexical-binding: t; -*-

;; Author: Daniel Thoren <danne_thoren@hotmail.com>
;; Maintainer: Daniel Thoren <danne_thoren@hotmail.com>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; Version: 1.0

;;; Commentary:
;;
;; Util functions used by block comment mode.
;;

;;; Code:

(defun block-comment--move-line (count)
"Moves `point' COUNT lines up/down, keeping the column position

Parameters:
  COUNT: The number of lines to move `point'."
  (let (
        (column (current-column))
        )
    (forward-line count)
    (move-to-column column)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                              Message wrappers                             """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment--message (messageStr)
  "When not in unit tests, forward message to normal `message' function."
  (when (not (block-comment--unit-tests-running))
    (message messageStr)
    )
  )

(defun block-comment--error (errMsg &optional errEcho)
  "Print error with special formatting when not in unit tests.

Prints error messages in red with 'Error: ' prefix. Does not
print when in unit tests."
  (when (not (block-comment--unit-tests-running))

    (unless errEcho (setq errEcho errMsg))

    ;; Format strings
    (setq errMsg (concat "Error: " errMsg))
    (setq errMsg (propertize errMsg 'face 'error))
    (setq errEcho (propertize errEcho 'face 'error))

    ;; Print echo message to Echo area in red
    (let ((message-log-max nil))
      (message errEcho)
      )

    ;; Print message to Messages buffer in red
    (with-current-buffer (messages-buffer)
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert errMsg)
        )
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                       Unit test related functions                         """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment--unit-tests-running ()
  "Check if the current context is running in the unit tests

-> Return: t if running unit tests, else nil"
  (and (boundp 'block-comment--unit-tests)
       block-comment--unit-tests)
  )

(provide 'block-comment-util)

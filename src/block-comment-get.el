;;; block-comment-get.el --- Getter functions, extracting information from the buffer  -*- lexical-binding: t; -*-

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
;; Getter functions used to parse and get information from the buffer contents.
;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                            Get functions                                  """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment--get-indent-level (&optional prefix)
"Returns the indentation level (column) of the current comment line.

Parameters:
  PREFIX: The prefix to look for.
          Default: `block-comment-prefix'.

-> Returns: The current indentation level."
  (unless prefix (setq prefix block-comment-prefix))

  (save-excursion
    (block-comment--jump-to-comment-start prefix)
    (current-column))
  )

(defun block-comment--get-line-count-in-buffer ()
  "Get the number of lines in the current buffer

-> Return: The number of lines in the current buffer"
  (count-lines (point-min) (point-max))
  )

(defun block-comment--get-widest-comment-text ()
  "Get the widest comment text from the current block comment.

-> Returns: The width of the widest comment text."
  (let (
        (widest-width 0)
        (curr-width 0)
        (at-top nil)
        )

    (save-excursion
      ;; Jump to the last non-postfix line in the block comment
      (block-comment--jump-to-last-comment-line 0 t)

      ;; Get widest comment text
      (while (and (not at-top)
                  (block-comment--is-body nil))

        (setq curr-width (block-comment--get-comment-text-width))
        (when (> curr-width widest-width)
          (setq widest-width curr-width))

        ;; Check if at top after performing logic in order to process the top
        ;; line before breaking the while loop
        (setq at-top (block-comment--is-at-buffer-top))

        ;; Move up one line
        (forward-line -1)
        )
      )

    ;; Return widest width
    widest-width
    ) ;; End let
  )

(defun block-comment--get-rightmost-comment-text-column ()
  "Get the column of the rightmost user text char in the current block comment.

-> Return: The column of the rightmost user text position."
  (let (
        (rightmost-column 0)
        (curr-column 0)
        (at-top nil)
        )

    (save-excursion
      ;; Jump to the last non-postfix line in the block comment
      (block-comment--jump-to-last-comment-line 0 t)

      ;; Get right most text column position
      (while (and (not at-top)
                  (block-comment--is-body nil))

        (block-comment--jump-to-last-char-in-body 0)
        (setq curr-column (current-column))
        (when (> curr-column rightmost-column)
          (setq rightmost-column curr-column))

        ;; Check if at top after performing logic in order to process the top
        ;; line before breaking the while loop
        (setq at-top (block-comment--is-at-buffer-top))

        ;; Move up one line
        (forward-line -1)
        )
      )

    ;; Return rightmost column
    rightmost-column
    ) ;; End let
  )

(defun block-comment--get-comment-width ()
  "Get the width of the entire block comment line at `point'.

Get the width of the entire comment line, meaning the entire
width from first char of prefix, to last char of postfix. This
function works on both a comment line, and a pre/post amble line

-> Return: The width of the current comment line."
  (let* (
         (comment-start 0)
         (comment-end 0)
         )

    (save-excursion
      (setq comment-start (block-comment--jump-to-comment-start))
      (setq comment-end (block-comment--jump-to-comment-end 0))
      )

    (- comment-end comment-start)
    )
  )

(defun block-comment--get-body-width (&optional prefix postfix)
"Returns the width of the block comment body at `point'.

The width is defined as the distance between the start & end of
the user text retion.

Parameters:
  PREFIX : The prefix to look for.
           Default: `block-comment-prefix'
  POSTFIX: The postfix to look for.
           Default: `block-comment-postfix'

-> Return: The width of the block comment body."
  (unless prefix (setq prefix block-comment-prefix))
  (unless postfix (setq postfix block-comment-postfix))

  (let (
        (comment-start 0)
        (comment-end 0)
        )

    (save-excursion
      (setq comment-start (block-comment--jump-to-body-start block-comment-edge-offset
                                                             prefix))
      (setq comment-end (block-comment--jump-to-body-end block-comment-edge-offset
                                                         postfix))
      )
    (- comment-end comment-start)
    )
  )

(defun block-comment--get-comment-text-width ()
  "Get the width of the user text within the block comment.

-> Return: The width of the user text."
  (let (
        (text-start 0)
        (text-end 0)
        )

    (save-excursion
      ;; Jump to first text column position
      (block-comment--jump-to-first-char-in-body)
      (setq text-start (current-column))

      ;; Jump to last text column position, no offset
      (block-comment--jump-to-last-char-in-body 0)
      (setq text-end (current-column))
      ) ;; End save-excursion

    ;; Return text width
    (- text-end text-start)
    ) ;; End let
  )

(defun block-comment--get-line-prefix-postfix ()
  "Get the saved prefix & postfix for the line type at `point'.

-> Return: The (prefix, postfix) of the line type at `point'
                            as a cons-cell."
  (let (
        (prefix nil)
        (postfix nil)
        )
    ;; Select the current lines pre/postfix
    (cond ((block-comment--is-body)
           (progn
             (setq prefix block-comment-prefix)
             (setq postfix block-comment-postfix))
           )
          ((block-comment--is-enclose-top)
           (progn
             (setq prefix block-comment-enclose-prefix-top)
             (setq postfix block-comment-enclose-postfix-top))
           )
          ((block-comment--is-enclose-bot)
           (progn
             (setq prefix block-comment-enclose-prefix-bot)
             (setq postfix block-comment-enclose-postfix-bot))
           )
          )

    (cons prefix postfix)
    )
  )

(defun block-comment--get-line-prefix ()
  "Get the saved prefix for the line type at `point'.

-> Return: The prefix of the line type at `point'."
  (car (block-comment--get-line-prefix-postfix))
  )

(defun block-comment--get-line-postfix ()
  "Get the saved postfix for the line type at `point'.

-> Return: The postfix of the line type at `point'."
  (cdr (block-comment--get-line-prefix-postfix))
  )

(defun block-comment--get-column-from-marker (marker)
  "Get column from  the given MARKER.

-> Return: The column of the marker."
  (if (markerp marker)
    (save-excursion
      (goto-char marker)
      (current-column)
      )
    (error
     "block-comment--get-column-from-marker: Invalid argument, must be marker!")
    )
  )

(defun block-comment--get-text-boundry ()
  "Get the start & end position (column) of the text on the line at `point'.

Return: Cons cell containing the start & end column positions"
  (let (
        (text-start nil)
        (text-end nil)
        )
    (if (and (block-comment--is-body)
             (block-comment--is-user-comment))
        (progn
          (save-excursion
            (block-comment--jump-to-last-char-in-body)
            (setq text-end (current-column))

            (block-comment--jump-to-first-char-in-body)
            (setq text-start (current-column))))
      )

    (cons text-start text-end)
    )
  )

(provide 'block-comment-get)

;;; block-comment-jump-to.el --- Jump to different positions of the block comment  -*- lexical-binding: t; -*-

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
;; Jump to utility functions used by the `block-comment-mode'. The
;; functions jump to various places within the block comment.
;;

;;; Code:

(defun block-comment--jump-to-starting-pos ()
  "Jump to the starting position of the current comment line.

This function assumes that the current line holds a block comment.
If there is a user text inside the block, it will always jump to the end of
the text. Otherwise, the behaviour will depend on wether centering mode is
enabled or not. If it is, then it will jump to the center of the comment body,
else to the beginning of the comment body.

-> Return nil"

  (if (block-comment--is-user-comment)
      (block-comment--jump-to-last-char-in-body)
    (if block-comment-centering--enabled
        (block-comment--jump-to-body-center)
      (block-comment--jump-to-body-start)
      )
    )

  nil
  )

(defun block-comment--jump-to-previous-text-column (&optional end)
  "Jump to the same column as the text block in the previous comment line.

  If param END is set to t, then jump to same column as the end
  of the user text on the previous line."
  (let* (
         (prev-block-start nil)
         (prev-comment-column nil)
         (prev-text-offset nil)
         )

    ;; Get previous text block offset from comment start
    (save-excursion
      (forward-line -1)
      (block-comment--jump-to-comment-start)
      (setq prev-block-start (current-column))

      (if end
          (block-comment--jump-to-last-char-in-body)
        (block-comment--jump-to-first-char-in-body))

      (setq prev-comment-column (current-column))
      (setq prev-text-offset (- prev-comment-column prev-block-start))
      )

    ;; Move to same position on current line
    (block-comment--jump-to-comment-start)
    (forward-char prev-text-offset)
    )
  )

(defun block-comment--jump-to-comment-start (&optional prefix)
  "Jump to block comment start, the first char of the prefix.

Parameters:
  PREFIX : The prefix to look for.
           Default: `block-comment-prefix'.

-> Return: The marker position of the comment start."

  (unless prefix (setq prefix (block-comment--get-line-prefix)))
  (block-comment--jump-to-body-start (- 0 (string-width prefix)) prefix)
  (point-marker)
  )

(defun block-comment--jump-to-comment-end (&optional offset postfix)
  "Jump to block comment end, the char directly after after the postfix.

Parameters:
  OFFSET : Offsets the end position of the jump. Positive values puts `point'
           closer to the comment body (to the left).
           Default: 1.
  POSTFIX: The postfix to look for.
           Default: `block-comment-postfix'.

-> Return: The marker position of the comment end."

  (unless offset (setq offset 1))
  (unless postfix (setq postfix (block-comment--get-line-postfix)))

  (block-comment--jump-to-body-end (- 0 (+ (string-width postfix) offset)) postfix)
  (point-marker)
  )

(defun block-comment--jump-to-body-center ()
  "Jump to the center of the block comment body.

-> Return: The final column position."
  (let (
        (start-point 0)
        (end-point 0)
        (line-width 0)
        (middle-point 0)
        )

    ;; Set line width for this line
    (save-excursion

      (block-comment--jump-to-comment-start)
      (setq start-point (current-column))

      (block-comment--jump-to-comment-end)
      (setq end-point (current-column))
      )
    (setq line-width (- end-point start-point))
    (setq middle-point (/ line-width 2))

    (when (> middle-point 0)
      (block-comment--jump-to-comment-start)
      (forward-char middle-point)
      )

    (current-column)
    )
  )

(defun block-comment--jump-to-body-start (&optional edge-offset prefix)
  "Jump to the start of block comment body, the char after the prefix.

Params:
  EDGE-OFFSET: The offset from the block comment prefix. Positive values puts
               `point' closer to the comment body (to the right).
               Default: `block-comment-edge-offset'.
  PREFIX     : The prefix to look for.
               Default: `block-comment-prefix'.

-> Return : The marker position of the body start."

  (unless edge-offset (setq edge-offset block-comment-edge-offset))
  (unless prefix (setq prefix (block-comment--get-line-prefix)))

  (let (
        (start-pos (point-marker))
        (line-end (line-end-position))
        )
    (beginning-of-line)

    ;; Place point at end of prefix if a prefix is found
    (if (search-forward prefix line-end t)
        (forward-char edge-offset)
      (goto-char start-pos))
    )
  (point-marker)
  )

(defun block-comment--jump-to-body-end (&optional edge-offset postfix)
  "Jump to the end of block comment body, the char before the prefix.

Params:
  EDGE-OFFSET : The offset from the block comment postfix. Positive values puts
                `point' closer to the comment body (to the left).
                Default: `block-comment-edge-offset'.
  POSTFIX     : The postfix to look for.
                Default: `block-comment-postfix'.

-> Return : The marker position of the body end."

  (unless edge-offset (setq edge-offset block-comment-edge-offset))
  (unless postfix (setq postfix (block-comment--get-line-postfix)))

  (let (
        (start-pos (point-marker))
        (line-start (line-beginning-position))
        )
    (end-of-line)

    ;; Place point at start of postfix if a postfix is found
    (if (search-backward postfix line-start t)
        (backward-char (+ edge-offset 1))
      (goto-char start-pos))
    )
  (point-marker)
  )

(defun block-comment--jump-to-first-char-in-body (&optional offset)
  "Jump to the first char in the user text.

Params:
  OFFSET : Offsets the end position of the jump.
           Positive values offsets `point' to the right.
           Default: 0.

-> Return : The number of fill characters on the left side of user text."
  (unless offset (setq offset 0))

  (let (
        (body-start-pos nil)    ; Start of block-comment body
        (comment-start-pos nil) ; Start of user comment
        )

    (beginning-of-line)
    ;; Find start position in block comment
    (block-comment--jump-to-body-start 0)

    ;; Set start of block-comment body
    (setq body-start-pos (current-column))

    (skip-syntax-forward " " (line-end-position))

    ;; Set start of user comment
    (setq comment-start-pos (current-column))

    (forward-char offset)

    ;; Return remaining space between user comment and start of
    ;; block-comment body
    (- comment-start-pos body-start-pos)
    )
  )

(defun block-comment--jump-to-last-char-in-body (&optional offset)
  "Jump to the last char in the user text.

Params:
  OFFSET : Offsets the end position of the jump.
           Positive values offsets `point' to the left.
           Default: 1.

-> Return : The number of fill characters on the right side of user text."
  (let (
        (body-end-pos nil)    ; End of block-comment body
        (comment-end-pos nil) ; End of user comment
        )
    (unless offset (setq offset 1))

    (end-of-line)
    ;; Find end position in block comment
    (block-comment--jump-to-body-end 0)

    ;; Set end of block-comment body
    (setq body-end-pos (current-column))

    ;; Jump back to character pos right after last char in body
    (skip-syntax-backward " " (line-beginning-position))
    ;; Jump back one more to stand on last char in body
    (backward-char 1)
    ;; Jump forward by offset
    (forward-char offset)

    ;; Set end of user comment
    (setq comment-end-pos (current-column))

    ;; Return remaining space between user comment and end of block-comment body
    (- body-end-pos comment-end-pos)
    )
  )

(defun block-comment--jump-to-last-comment-line (&optional offset stop-before-postfix)
  "Jump to last block comment line.

Function moves down until the line no longer follows the block
comment format. Then it stops and moves up one line, thus
stopping at the last line of the block comment.

Parameters:
  OFFSET             : Offsets the end position of the jump. Positive values puts `point'
                       furhter down, negative further up.
                       Default: 1.
  STOP-BEFORE-POSTFIX: If t, stop on last block comment line.
                       Default: nil."
  (unless offset (setq offset 0))

  (let (
        (is-body nil)
        (is-enclose-top nil)
        (is-enclose-bot nil)
        (lines-in-buffer (block-comment--get-line-count-in-buffer))
        (at-bottom nil)
        )
    ;; Move to line below bottom of block comment
    (while (progn
             ;; Move down one line
             (forward-line 1)

             ;; Check if this is body or enclose
             (setq is-body (block-comment--is-body nil))
             (setq is-enclose-top (block-comment--is-enclose-top nil))
             (unless stop-before-postfix
               (setq is-enclose-bot (block-comment--is-enclose-bot nil)))

             (setq at-bottom (block-comment--is-at-buffer-bot lines-in-buffer))

             ;; Exit if not in comment or if at bottom of buffer
             (and (not at-bottom)
                  (or is-body is-enclose-top is-enclose-bot))
             )
      )

    ;; Move back up to last comment line if necessary
    (unless at-bottom (forward-line -1))
    )

  ;; Move according to offset
  (block-comment--move-line offset)
  )

(provide 'block-comment-jump-to)

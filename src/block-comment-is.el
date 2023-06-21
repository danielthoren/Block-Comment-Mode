;;; block-comment-is.el --- Boolean checks based on buffer contents  -*- lexical-binding: t; -*-

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
;; Is functions used to check the current state of the block comment
;; in the buffer.
;;

;;; Code:

(defun block-comment--is-at-buffer-top ()
  "Check if `point' is at the first line in the current buffer.

-> Return: t if current pos is at the first line, else nil."
  (equal (line-number-at-pos) 1)
  )

(defun block-comment--is-at-buffer-bot (&optional lines-in-buffer)
  "Check if `point' is at the last line in the current buffer.

Params:
  LINES-IN-BUFFER : The number of lines in the current buffer. Can be sent to
                    minimize the the number of times line count need to be
                    calculated when called in a loop.
-> Return: t if `point' is at the last line, else nil."
  (unless lines-in-buffer (setq lines-in-buffer (block-comment--get-line-count-in-buffer)))
  (equal (line-number-at-pos) lines-in-buffer)
  )

(defun block-comment--is-centering-line (&optional tolerance)
"Checks whether the current block comment line is centering or non-centering.

This function determines if the current line of the block comment
is centering based on the margins. If the left margin is larger
than (edge-offset + 1) and the difference between the left/right
margins is less than the specified TOLERANCE, the line is
considered centering.

Parameters:
  TOLERANCE: Determines how much off-center the text is allowed to be.
             Default: 2.

-> Returns: t if the text is centered, nil otherwise."
  (unless tolerance (setq tolerance 2))

  (save-excursion
    (let (
          (begin-width (block-comment--jump-to-first-char-in-body))
          (end-width (block-comment--jump-to-last-char-in-body 0))
          )
      ;; If diff between begin/end width is smaller than x, then assume
      ;; that we are in centering mode
      (and (> begin-width (+ block-comment-edge-offset 1))
           (> tolerance (abs (- begin-width end-width))))
      )
    )
  )

(defun block-comment--is-empty-line (&optional pos)
  "Check if the line at POS is empty, only containing ' ' characters.

Parameters:
  POS: The position at which to check if the line is empty.
       Default: `point'.

-> Return: t if line is empty, else nil"
  (save-excursion
    (goto-char (or pos (point)))
    (beginning-of-line)
    (looking-at-p "[[:space:]]*$")
    )
  )

(defun block-comment--is-point-right-of-comment ()
  "Checks if `point' is to the right of the user text.

-> Return: t if `point' is to the right of the user text, else nil."
  (save-excursion
    (let (
          (current-pos (current-column))
          (text-end (progn
                      (block-comment--jump-to-last-char-in-body)
                      (current-column)))
          )
      (>= current-pos text-end)
      )
    )
  )

(defun block-comment--is-user-comment ()
  "Checks if the block-comment-body at `point' contains a user comment.

-> Return: t if it does, else nil."
  (let (
        (body-end nil)
        )
    (save-excursion
      (setq body-end (block-comment--jump-to-body-end))
      (block-comment--jump-to-body-start)
      (skip-syntax-forward " " body-end)

      (not (equal (point-marker)
                  body-end))
      )
    )
  )

(defun block-comment--is-enclose-top (&optional inside-body)
  "Check if line at `point' follows the top enclose format.

Uses the globally stored parameters for prefix, fill and postfix
parameters.

Parameters:
  INSIDE-BODY: t if point is required to be inside of the body.
               Default: nil.

-> Return: t if line looks like top enclose, else nil."
  (let (
        (match-signature nil)
        (has-body-beneath nil)
        )

    (setq match-signature (block-comment--is-enclose block-comment-enclose-prefix-top
                                                     block-comment-enclose-fill-top
                                                     block-comment-enclose-postfix-top
                                                     inside-body))
    (save-excursion
      (forward-line 1)
      (setq has-body-beneath (block-comment--is-body))
      )

    (and match-signature has-body-beneath)
    )
  )

(defun block-comment--is-enclose-bot (&optional inside-body)
  "Check if line at `point' follows the bot enclose format.

Uses the globally stored parameters for prefix, fill and postfix
parameters.

Parameters:
  INSIDE-BODY: t if point is required to be inside of the body.
               Default: nil.

-> Return: t if line looks like bot enclose, else nil."
  (let (
        (match-signature nil)
        (has-body-beneath nil)
        (has-body-above nil)
        )
    (setq match-signature (block-comment--is-enclose block-comment-enclose-prefix-bot
                                                     block-comment-enclose-fill-bot
                                                     block-comment-enclose-postfix-bot
                                                     inside-body))

    (save-excursion
      (forward-line 1)
      (setq has-body-beneath (block-comment--is-body))
      )

    (save-excursion
      (forward-line -1)
      (setq has-body-above (block-comment--is-body))
      )

    (and match-signature has-body-above (not has-body-beneath))
    )
  )

(defun block-comment--is-body (&optional inside-body)
  "Check if line at `point' follows the comment line format.

Uses the globally stored parameters for prefix, fill and postfix
parameters.

Parameters:
  INSIDE-BODY: t if point is required to be inside of the body.
               Default: nil.

-> Return: t if line looks like comment line, else nil."
  (unless inside-body (setq inside-body nil))

  (block-comment--is-comment block-comment-prefix
                             block-comment-fill
                             block-comment-postfix
                             inside-body))

(defun block-comment--is-enclose (prefix fill postfix &optional inside-body)
  "Check if line at `point' follows the enclose format with the given parameters.

Parameters:
  PREFIX     : The prefix to look for.
  FILL       : The fill to look for.
  POSTFIX    : The postfix to look for.
  INSIDE-BODY: t if point is required to be inside of the body.
               Default: nil.

-> Return: t if line looks like comment line, else nil."
  (let (
        (is-comment (block-comment--is-comment prefix fill postfix inside-body))
        (is-enclose nil)
        (block-start nil)
        (block-end nil)
        (enclose-body nil)
        (enclose-body-template nil)
        (encountered-error nil)
        )
    (when is-comment
      (condition-case nil
          (save-excursion
            ;; Make sure there is only fill characters in-between prefix/postfix
            (beginning-of-line)
            (skip-syntax-forward " " (line-end-position))
            (forward-char (+ 1 (string-width prefix)))
            (setq block-start (point-marker))

            (end-of-line)
            (skip-syntax-backward " " (line-beginning-position))
            (backward-char (+ 2 (string-width postfix)))
            (setq block-end (point-marker))
            )
        ((end-of-buffer beginning-of-buffer)
         (setq encountered-error t)
         (block-comment--error "block-comment--is-enclose: Encountered end-of-buffer" "BC: end-of-buffer")
         )
        )

      (unless encountered-error
        (setq enclose-body (buffer-substring block-start block-end))
        (setq enclose-body-template (make-string (string-width enclose-body)
                                                 (string-to-char fill)))

        ;; If the entire enclose body contains the fill character,
        ;; the current line containes enclose, return t, else nil
        (when (string= enclose-body-template enclose-body)
          (setq is-enclose t)
          )
        )
      )
    ;; Return if this is enclose
    is-enclose
    )
  )

(defun block-comment--is-comment (prefix fill postfix &optional inside)
  "Check if line at `point' follows the comment line format with the given parameters.

Parameters:
  PREFIX     : The prefix to look for.
  FILL       : The fill to look for.
  POSTFIX    : The postfix to look for.
  INSIDE-BODY: t if point is required to be inside of the body.
               Default: nil.

-> Return: t if line looks like comment line, else nil."
  (unless inside (setq inside nil))

  (let (
        (read-prefix-pos nil)   ; Position of current line:s prefix
        (read-postfix-pos nil)  ; Position of current line:s postfix
        (point-in-body t)       ; If point is inside body.
        )
    ;; Check if prefix is present on this line
    (save-excursion
      (beginning-of-line)
      (setq read-prefix-pos
            (search-forward
             (concat prefix fill)
             (line-end-position)
             t)))

    ;; Check if postfix is present on this line
    (save-excursion
      (end-of-line)
      (setq read-postfix-pos
            (search-backward
             (concat fill postfix)
             (line-beginning-position)
             t)))

    ;; If inside-body is true, check if point is inside body
    (when (and
           inside
           read-prefix-pos
           read-postfix-pos)

      (setq point-in-body (and
                           (> (point-marker) (+ read-prefix-pos block-comment-edge-offset))
                           (< (point-marker) (- read-postfix-pos block-comment-edge-offset))
                           )))

    ;; Return value, t if in block comment line, else nil
    (and read-prefix-pos
         read-postfix-pos
         point-in-body)
    )
  )

(provide 'block-comment-is)
